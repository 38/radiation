# The reason why there's such  script is the best way for the indentation is 
# mixing spaces with tabs. A lot of people suggest that we should totally use space anyway 
# However, this increase the size of the source code and makes other uncomfortable 
# with the "hard indentation". When I have a smaller screen, I need the TABSTOP become
# smaller, however the space approach fail to do this.
# So the good way for indentation is use tabs for indentation, use space for alignment. 
# In this way, the code is perfectly indented, and people are free to change the TABSTOP
# This tool is written for the reason above
import sys
import re
import os

def format(fp):
	comment = False
	string  = False
	char   = False
	escape = False
	idlevel = 0
	recent = "  "
	result = ""
	changed = False
	for line in fp:
		spaces = 0
		for ch in line:
			if ch == ' ': spaces += 1
			elif ch == '\t': spaces += 4
			else: break
		level = idlevel - (len(line.strip()) != 0 and line.strip()[0] == '}')
		spaces -= level * 4
		if spaces < 0: spaces = 0
		header = '\t'*level + ' '*spaces  
		if string: 
			header = re.match(r'^[ \t]*',  line) # Do not touch the string 
			header = header.group() if header else ""
		for ch in line:
			recent = (recent + ch)[1:]
			if not comment and not string and not char:
				if recent == "//": break
				elif recent == "/*": comment = True 
				elif recent[1] == "\"": string = True
				elif recent[1] == "\'": char = True
			elif comment:
				if recent == "*/": comment = False
			elif string:
				if not escape and recent[1] == "\\": escape = True
				elif escape: escape = False
				elif recent[1] == '"': string = False
			elif char:
				if not escape and recent[1] == "\\": escape = True
				elif escape: escape = False
				elif recent[1] == "\'": char = False
			if not comment and not string:
				if(ch == '{'): idlevel += 1
				if(ch == '}'): idlevel -= 1
		result += header + line.strip() + "\n"
		if header + line.strip() + "\n" != line: changed = True
	fp.close()
	return changed,result
				

fn=r'.*\.(c|h|cpp|cxx|hpp|scala)$'
ts = 4

for root, _, files in os.walk("."):
	for f in files:
		if not re.match(fn,f): continue
		path="%s/%s"%(root,f)
		ch, result = format(file(path))
		if ch: print "Info: file %s has been changed"%path
		f = file(path, "w")
		f.write(result)
		f.close()

