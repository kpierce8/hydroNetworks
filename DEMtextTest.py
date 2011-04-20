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


myPoints = "C:\data\projectslocal\jpoints.txt"
#myRaster = "Database Connections\Connection to 198.238.177.37.sde\GEOLIB.DBO.DEM10"
myRaster = "E:\spatialdata\wadem10\wadem10"


##buf_temp50 = "C:\\data\\WCCheck\\buffers\\strbuf50_" + wria + ".shp"
##buf_temp100 = "C:\\data\\WCCheck\\buffers\\strbuf100_" + wria + ".shp"


#Iteration Logic
##SSize = 200
##Long = 0
n = 0
#SQLExp = ' "FID" > ((%d + %d)  * %d) AND "FID" <= ((%d + 1 + %d) * %d)' % (n, Long, SSize, n, Long, SSize) 
outTable = "C:\\data\\projectslocal\\segments\\DEMtextPTS_.dbf"
#gp.addfield(myPoints,'DEM10','FLOAT')

gp.Sample_sa(myRaster,myPoints , outTable, "NEAREST")
#gp.MakeFeatureLayer_management(myPoints, 'targ_lay', SQLExp)
#gp.CopyFeatures_management('targ_lay','c:/pytest/output/lay_test1.shp')
##nrecords = gp.getcount(myPoints)
##nrecords = 200
##print time.ctime()
##while n*SSize < nrecords:
##    bob = time.time()
##    SQLExp = ' [OBJECTID] >= ((%d + %d)  * %d) AND [OBJECTID] < ((%d + 1 + %d) * %d)' % (n, Long, SSize, n, Long, SSize) 
##    gp.MakeFeatureLayer_management(myPoints, 'targ_lay', SQLExp)
    #Check four different buffer regions below for similarity [should recode as FUNCTION]
##    print 'iteration =' + str(n)
    #gp.MakeFeatureLayer_management(myRaster, 'compare_lay')
    #gp.CalculateField_management('targ_lay', 'XLand', 'x', 'PYTHON', 'x = 0')



#Iterated function

    #gp.SelectLayerByLocation_management('targ_lay', 'CROSSED_BY_THE_OUTLINE_OF', 'compare_lay')
    #gp.CalculateField_management('targ_lay', 'XLand', 'x', 'PYTHON', 'x = 1')   
    #Finished buffer check for polylines
##    n +=1
##    print time.ctime()

##end of while    


