import fnmatch
import os
import glob
import sys

success = '\033[92m'
fail = '\033[91m'

def run_test(file):
	cmd = "./oncotime %s" % file
	# print "> %s : " % file
	os.system(cmd)

def valid():
	print ("Running tests that should be VALID...")
	for root, dirnames, filenames in os.walk('programs/valid'):
	        for filename in filenames:
	        	if not filename.endswith(('.pretty.onc','.grp')):
	        		run_test (os.path.join(root, filename))
	for root, dirnames, filenames in os.walk('programs/examples'):
	        for filename in filenames:
	        	if not filename.endswith(('.pretty.onc','.grp')):
	        		run_test (os.path.join(root, filename))
	print

def invalid():
	print ("Running tests that should be INVALID...")
	for root, dirnames, filenames in os.walk('programs/invalid'):
	        for filename in filenames:
	        	if not filename.endswith(('.pretty.pretty.onc','.grp')):
	        		run_test (os.path.join(root, filename))

if (len(sys.argv) == 1):
	valid()
	invalid()
else:
	if (sys.argv[1] == "-i"):
		invalid()
	elif (sys.argv[1] == "-v"):
		valid()
	else:
		print ("Script failed, man.")
		sys.exit()
