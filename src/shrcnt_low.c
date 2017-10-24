/* -------------------------------------------------------------------- */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/mman.h>
#include <semaphore.h>
#include <fcntl.h>
#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

/* -------------------------------------------------------------------- */
#define NEW(T)    ((T*) calloc(1u, sizeof (T)))
#define DELETE(P) (free(P))

#if !defined(PROT_RDWR)
# define PROT_RDWR (PROT_READ | PROT_WRITE)
#endif

#if !defined(S_IRWUSR)
# define S_IRWUSR (S_IRUSR | S_IWUSR)
#endif

/* -------------------------------------------------------------------- */
typedef struct cnt {
    uint32_t cn_value;
} cnt_t;

struct shrcnt {
    int    sc_fd;
    cnt_t *sc_mem;
    sem_t *sc_sem;
    int    sc_flags;
    char  *sc_nm;
};

typedef uint32_t sfc_t;

#define SFC_CREATE ((sfc_t) 0x0001)
#define SFC_UNLINK ((sfc_t) 0x0002)

/* -------------------------------------------------------------------- */
static struct shrcnt *shrcnt_alloc(void) {
    struct shrcnt *the = NEW(struct shrcnt);
    
    if (the != NULL) {
        memset(the, 0, sizeof(struct shrcnt));
        the->sc_fd = -1;
    }
    
    return the;
}

/* -------------------------------------------------------------------- */
static void shrcnt_destroy(struct shrcnt *the) {
    if (the->sc_mem != NULL && the->sc_mem != MAP_FAILED)
        (void) munmap(the->sc_mem, sizeof(cnt_t));
    if (the->sc_sem != NULL && the->sc_sem != SEM_FAILED)
        (void) sem_close(the->sc_sem);
    if (the->sc_fd >= 0) {
        (void) close(the->sc_fd);
    }
    if (the->sc_nm != NULL) {
        if ((the->sc_flags & SFC_UNLINK)) {
            (void) shm_unlink(the->sc_nm);
            (void) sem_unlink(the->sc_nm);
        }
        free(the->sc_nm);
    }
    DELETE(the);
}

/* -------------------------------------------------------------------- */
static struct shrcnt *shrcnt_create(const char *nm, sfc_t flags) {
    struct shrcnt *the = shrcnt_alloc();
    int oflags = (flags & SFC_CREATE) ? O_CREAT : 0;
    
    if (the == NULL)
        goto bailout;

    if ((the->sc_nm = strdup(nm)) == NULL)
        goto bailout;
    the->sc_flags = flags;
    
    if ((flags & SFC_CREATE)) {
        (void) shm_unlink(nm);
        (void) sem_unlink(nm);
    }

    if ((the->sc_fd = shm_open(nm, oflags | O_RDWR, S_IRWUSR)) < 0)
        goto bailout;
    
    if (ftruncate(the->sc_fd, sizeof(cnt_t)) < 0)
        goto bailout;
    
    the->sc_mem = mmap(NULL, sizeof(cnt_t), PROT_RDWR, MAP_SHARED, the->sc_fd, 0);
    if (the->sc_mem == MAP_FAILED)
        goto bailout;

    if ((the->sc_sem = sem_open(nm, oflags, S_IRWUSR, 1)) == SEM_FAILED)
        goto bailout;
    
    return the;
    
bailout:
    if (the != NULL)
        shrcnt_destroy(the);
    return NULL;
}

/* -------------------------------------------------------------------- */
static int shrcnt_modify(struct shrcnt *the, int32_t offset, uint32_t *value) {
    if (sem_wait(the->sc_sem) < 0)
        return -1;
    the->sc_mem->cn_value += offset;
    if (value != NULL)
        *value = the->sc_mem->cn_value;
    if (sem_post(the->sc_sem) < 0)
        return -1;
    return 0;
}

/* -------------------------------------------------------------------- */
static int shrcnt_get(struct shrcnt *the, uint32_t *out) {
    if (sem_wait(the->sc_sem) < 0)
        return -1;
    *out = the->sc_mem->cn_value;
    if (sem_post(the->sc_sem) < 0)
        return -1;
    return 0;
}

/* -------------------------------------------------------------------- */
static void caml_shrcnt_finalize(value the);

static struct custom_operations counter_ops = {
    "nu.strub.counter",
    caml_shrcnt_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

#define EXN_COUNTER "nu.strub.counter.exn"

/* -------------------------------------------------------------------- */
#define Counter_val(v) (*((struct shrcnt **) Data_custom_val(v)))

/* -------------------------------------------------------------------- */
static CAMLprim void caml_shrcnt_failure(void) {
    caml_raise_constant(*caml_named_value(EXN_COUNTER));
}

/* -------------------------------------------------------------------- */
static CAMLprim void caml_shrcnt_finalize(value the) {
    CAMLparam1(the);
    
    if (Counter_val(the) != NULL)
        shrcnt_destroy(Counter_val(the));
    CAMLreturn0;
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_create(value nm, value flags) {
    CAMLparam2(nm, flags);
    CAMLlocal1(v);

    v = caml_alloc_custom(&counter_ops, sizeof(struct shrcnt *), 0, 1);
    if ((Counter_val(v) = shrcnt_create(String_val(nm), Int_val(flags))) == NULL)
        caml_shrcnt_failure();
    CAMLreturn(v);
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_dispose(value the) {
    CAMLparam1(the);

    if (Counter_val(the) != NULL) {
        shrcnt_destroy(Counter_val(the));
        Counter_val(the) = NULL;
    }
    CAMLreturn(Val_unit);
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_disposed(value the) {
    CAMLparam1(the);
    CAMLreturn(Val_bool(Counter_val(the) == NULL));
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_id(value the) {
    CAMLparam1(the);

    if (Counter_val(the) == NULL)
        caml_shrcnt_failure();
    CAMLreturn(caml_copy_string(Counter_val(the)->sc_nm));
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_get(value the) {
    CAMLparam1(the);
    uint32_t cnt;

    if (Counter_val(the) == NULL)
        caml_shrcnt_failure();
    if (shrcnt_get(Counter_val(the), &cnt) < 0)
        caml_shrcnt_failure();
    CAMLreturn (caml_copy_int32(cnt));
}

/* -------------------------------------------------------------------- */
CAMLprim value caml_shrcnt_update(value the, value offset) {
    CAMLparam2(the, offset);
    uint32_t cnt;

    if (Counter_val(the) == NULL)
        caml_shrcnt_failure();
    if (shrcnt_modify(Counter_val(the), Int32_val(offset), &cnt) < 0)
        caml_shrcnt_failure();
    CAMLreturn(caml_copy_int32(cnt));
}

