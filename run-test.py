#! /usr/bin/env python

# --------------------------------------------------------------------
import sys, os, re, errno, time, subprocess as sp, logging

# --------------------------------------------------------------------
ROOT = os.path.realpath(os.path.dirname(__file__))
EXMP = os.path.join(ROOT, 'examples')
OUTP = os.path.join(ROOT, 'output')

devnull = open(os.path.devnull, 'w')

# --------------------------------------------------------------------
def _main():
    logging.basicConfig(stream = sys.stderr, level = logging.INFO)

    if len(sys.argv)-1 not in (0, 1):
        print >>sys.stderr, "Usage: run-test [regexp]"
        exit(1)

    flt = (sys.argv[1:2] or [r'.*'])[0]
    flt = re.compile(flt)

    def _validate_path(name):
        fullpath = os.path.join(EXMP, name)
        if os.path.isdir(fullpath):
            m = re.search(r'^order(\d+)', name)
            if m is not None:
                return (name, int(m.groups()[0]))
        return None

    def _expend(order):
        files = os.listdir(os.path.join(EXMP, order[0]))
        files = [x for x in files if os.path.splitext(x)[1] == '.ec']
        files = [x for x in files if flt.search(x)]
        return (order, sorted(files))

    try:
        os.makedirs(OUTP)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise

    dirs = [_validate_path(x) for x in os.listdir(EXMP)]
    dirs = [x for x in dirs if x is not None]
    dirs = sorted(dirs)
    dirs = [_expend(x) for x in dirs]

    for (dir_, order), filenames in dirs:
        for filename in filenames:
            cmd  = ['easycrypt']
            cmd += ['-I', os.path.join(EXMP)]
            cmd += ['-I', os.path.join(EXMP, dir_)]
            cmd += [os.path.join(EXMP, dir_, filename)]
            out  = 'order-%d-%s.err' % (order, os.path.splitext(filename)[0])
            out  = os.path.join(OUTP, out)

            logging.info('Running [%d/%s]' % (order, filename))

            with open(out, 'w') as stream:
                t1 = time.time()
                sp.call(cmd,
                        stdout = devnull,
                        stderr = stream)
                t2 = time.time()
                print >>stream, "TIME: %.3f" % (t2 - t1)

# --------------------------------------------------------------------
if __name__ == '__main__':
    _main()
