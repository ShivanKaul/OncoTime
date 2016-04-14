# Usage:
# python <script name> <filename>.py

import sys

f = open(sys.argv[1],'r')
filedata = f.read()
f.close()

newdata = filedata.replace("user='root', passwd='root'","user='520student', passwd='comp520'")

f = open(sys.argv[1],'w')
f.write(newdata)
f.close()
