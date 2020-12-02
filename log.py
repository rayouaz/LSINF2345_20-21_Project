from os import system as sys
import re
import statistics 

def launch(N):
    sys("erlc node.erl")
    sys("erlc tree.erl")
    sys("erlc project.erl")
    sys("erlc bootstrap_server.erl")
    sys("erlc functions.erl")
    strLaunch = "project:launch("+str(N)+")"
    strErl = "erl -noshell -eval "+ strLaunch+" -run init stop > log.txt"
    sys(strErl)
def computeMean(file):
    return 0
def computeDev(file):
    return 0

def makePlot(mean, dev):
    pass


#launch(15)
N = 15
#launch(N)
file = open("log.txt", 'r')
fileR = str(file.read())
source = fileR
p = re.compile('log::(.*)')
listC = [ [0 for i in range(N) ] for i in range(180)]
print(listC)
for log in re.findall(p, source):
    for i in range(180):
        logs = log.split(" ")
        print(logs[1])
        p2 = re.compile('{(.),')
        indegs = re.findall(p2, log)
        for indeg in indegs:
            if int(logs[1]) == i:
                listC[i][int(indeg)] = listC[i][int(indeg)] + 1 
print(listC)
mathFile = open("deployment.data", 'w')
count = 0
for cycle in listC:
    mean = statistics.mean(cycle)
    dev = statistics.stdev(cycle)
    mathFile.write(str(count) + " " + str(mean) + " " + str(dev) + "\n")
    count = count + 1
