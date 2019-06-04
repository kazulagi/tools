import os
import sys
import time

n=input("How many pictures are you want to take?")
print(n)
m=input("How long do you want to take pictures? (min)")
print(m)
fp=input("File path : ")
print("Total "+str( int(m)*int(n))+"pictures will be taken")
for i in range(int(m)*int(n)):
    num=str(i)
    command="raspistill -o shot"+fp+num+".jpg"
    os.system(command)
    print(command)
    print(int(60.0/float(n)))
    time.sleep( int(60.0/float(n))) 
