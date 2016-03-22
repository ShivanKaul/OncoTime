# import fnmatch
import os
# import glob
import sys

success = '\033[92m'
fail = '\033[91m'


def run_test(file, flag1="", flag2=""):
    print ('Running test ' + file + ':')

    cmd = "./oncotime %s %s %s" % (file, flag1, flag2)
    print ("Running command: " + cmd)
    # print "> %s : " % file
    os.system(cmd)


def valid():
    print ("Running tests that should be VALID...")
    for root, dirnames, filenames in os.walk('programs/valid'):
            for filename in filenames:
                if (filename.endswith(('.onc')) and
                    not filename.endswith(('.pretty.onc'))):

                    run_test(os.path.join(root, filename))
    for root, dirnames, filenames in os.walk('programs/examples'):
            for filename in filenames:
                if not filename.endswith(('.pretty.onc')):
                    run_test(os.path.join(root, filename))
    print


def invalid():
    print ("Running tests that should be INVALID...")
    for root, dirnames, filenames in os.walk('programs/invalid'):
            for filename in filenames:
                if not filename.endswith(('pretty.onc')):
                    run_test(os.path.join(root, filename))

if (len(sys.argv) == 1):
    valid()
    invalid()
else:
    if (sys.argv[1] == "-i"):
        invalid()
    elif (sys.argv[1] == "-v"):
        valid()
    else:
        if (len(sys.argv) == 3):
            run_test(sys.argv[1], sys.argv[2])
        elif (len(sys.argv) == 4):
            run_test(sys.argv[1], sys.argv[2], sys.argv[3])
        sys.exit()
