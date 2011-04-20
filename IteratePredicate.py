import sys, string, os, arcgisscripting, time
#print time.time() + 'Script2'
##wria = sys.argv[1]
##if wria == '#':
##    wria = '6'
    
#wria = '12'
# Create the Geoprocessor object
gp = arcgisscripting.create()

# Set the necessary product code
gp.SetProduct("ArcInfo")

# Check out any necessary licenses
gp.CheckOutExtension("spatial")
gp.OverwriteOutput = True
# Load required toolboxes...
##try:
gp.AddToolbox("C:/Program Files/ArcGIS/ArcToolbox/Toolboxes/Spatial Analyst Tools.tbx")
gp.AddToolbox("C:/Program Files/ArcGIS/ArcToolbox/Toolboxes/Conversion Tools.tbx")
gp.AddToolbox("C:/Program Files/ArcGIS/ArcToolbox/Toolboxes/Data Management Tools.tbx")
gp.AddToolbox("C:/Program Files/ArcGIS/ArcToolbox/Toolboxes/Analysis Tools.tbx")


compare_temp1 = "e:\\ForageFish\\Pierce\\sz_polys\\szline_to_poly_Featuretopoly2.shp"
targ_temp1 = "e:\\foragefish\\net400k.shp"

##buf_temp10 = "C:\\data\\WCCheck\\buffers\\strbuf10_" + wria + ".shp"
##buf_temp50 = "C:\\data\\WCCheck\\buffers\\strbuf50_" + wria + ".shp"
##buf_temp100 = "C:\\data\\WCCheck\\buffers\\strbuf100_" + wria + ".shp"


#Iteration Logic
SSize = 200
Long = 0
n = 0
#SQLExp = ' "FID" > ((%d + %d)  * %d) AND "FID" <= ((%d + 1 + %d) * %d)' % (n, Long, SSize, n, Long, SSize) 

##gp.addfield(targ_temp1,'CowStr24','LONG')


#gp.MakeFeatureLayer_management(targ_temp1, 'targ_lay', SQLExp)
#gp.CopyFeatures_management('targ_lay','c:/pytest/output/lay_test1.shp')
nrecords = gp.getcount(targ_temp1)
print time.ctime()
while n*SSize < nrecords:
    bob = time.time()
    SQLExp = ' "FID" >= ((%d + %d)  * %d) AND "FID" < ((%d + 1 + %d) * %d)' % (n, Long, SSize, n, Long, SSize) 
    gp.MakeFeatureLayer_management(targ_temp1, 'targ_lay', SQLExp)
    #Check four different buffer regions below for similarity [should recode as FUNCTION]
    print 'iteration =' + str(n)
    gp.MakeFeatureLayer_management(compare_temp1, 'compare_lay')
    gp.CalculateField_management('targ_lay', 'XLand', 'x', 'PYTHON', 'x = 0')
    gp.SelectLayerByLocation_management('targ_lay', 'CROSSED_BY_THE_OUTLINE_OF', 'compare_lay')
    gp.CalculateField_management('targ_lay', 'XLand', 'x', 'PYTHON', 'x = 1')   
    #Finished buffer check for polylines
    n +=1
    print time.ctime()

##end of while    


