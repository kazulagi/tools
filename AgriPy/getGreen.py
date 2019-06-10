from PIL import Image
import sys
import os

# This script is for pixcel-wise operations

# Reading file name (.jpg, .png ...etc.)
fp = input("ImageFile name : ")

# Importing image file
img_in = Image.open( str(fp) )

# Convert .jpg,.png ...etc. to RGB values
rgb_im = img_in.convert('RGB')

# get pixcel size (row and column)
size = rgb_im.size

print( str(size[0]), ' ',str(size[1])  )
itr = 0
width,height =img_in.size

# Name of ExportFile
outfile = open(fp+".txt","w")
count=0
outfile.write("Pixcel-x, Pixcel-y, R, G, B \n" )        
for i in range(width):
    for j in range(height):
        R,G,B=rgb_im.getpixel((i,j))
        # Please write some if-statement for pixcel-wise operation
        # This program outputs pixcel coordinates (x,y) and R,G,B

        #outfile.write(str(i)+"  "+str(j)+"  "+str(R)+"  "+str(G)+"  "+str(B)+"\n" )
        #er=R+G+B
        if int(G) >= int(R)           :
                count=count+1
        #    python_buffer.write(str(i)+'\t')
        #    itr=itr+1
        #    python_buffer.write(str(j)+'\n')
GreenRate=float(count)/float(width)/float(height)*100.0
print("Cover ratio is : "+str(GreenRate)+" %"+"\n")
outfile.write(str(GreenRate)+"%")
img_in.close()
outfile.close()
