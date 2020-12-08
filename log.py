from os import system as sys
import sys as sys2
import re
import statistics 

def launch(N,H,S,C):
    sys("erlc node.erl")
    sys("erlc tree.erl")
    sys("erlc project.erl")
    sys("erlc bootstrap_server.erl")
    sys("erlc functions.erl")
    strLaunch = "project:launch("+str(N)+","+str(H)+","+str(S)+","+str(C)+")"
    strErl = "erl -noshell -eval "+ strLaunch+" -run init stop > log.txt"
    sys(strErl)
def computeMean(file):
    return 0
def computeDev(file):
    return 0

def makePlot(mean, dev):
    pass


N = int(sys2.argv[1])
print(sys2.argv[1])
launch(sys2.argv[1], sys2.argv[2], sys2.argv[3], sys2.argv[4])
file = open("log.txt", 'r')
fileR = str(file.read())
source = fileR
source = source.replace("\n", "")
file = open("source.txt", 'w')
file.write(source)
print(source)
p = re.findall(re.compile('log::([<{}>, [\].0-9]*)'), source)
#e = p.group(1)
#print(e)
listC = [ [0 for i in range(N) ] for i in range(180)]
for log in p:
    for i in range(180):
            logs = log.split(" ")
            #print(logs)
            p2 = re.compile('{([0-9]*)')
            indegs = re.findall(p2, log)
            #print("el log: ",log)
            #print("indegs: ", indegs)
            #print(logs)
            #print(logs[2])
            for indeg in indegs:
                if int(logs[2]) == i:
                    #print(logs[1])
                    listC[i][int(indeg)] = listC[i][int(indeg)] + 1 
print(listC)
mathFile = open("healer_deployment2.data", 'w')
count = 0
print("shoooow")
print(listC)
for cycle in listC:
    if (count%20 == 0):
        mean = statistics.mean(cycle)
        dev = statistics.stdev(cycle)
        mathFile.write(str(count) + " " + str(mean) + " " + str(dev) + "\n")
    elif (count == len(listC)-1):
        mean = statistics.mean(cycle)
        dev = statistics.stdev(cycle)
        mathFile.write(str(count+1) + " " + str(mean) + " " + str(dev) + "\n")
    count = count + 1
