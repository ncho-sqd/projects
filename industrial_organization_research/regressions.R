#Weekend/day dummy generation 0-6 starting from Sunday for the entire nem$DATETIME vector
dummy.weekday = as.POSIXlt(nem$DATETIME)$wday
dummy.weekday[dummy.weekday==0|dummy.weekday==6]=1 #weekend is assigned value=1
dummy.weekday[dummy.weekday>=1&dummy.weekday<=6]=0 #weekday is assigned value=0

#####################
#####REGRESSION######
#####################

##PLAIN-VANILLA (without weekday/end dummy)
#Batch1 Spec1
QLD_reg_b1_s1_NSW = lm(nem$QLD.Price[1:575]~QLD.PRICE+QLD.DEMAND+QLD.WINDGEN+NSW.DEMAND)
NSW_reg_b1_s1_VIC = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+NSW.WINDGEN+VIC.DEMAND)
NSW_reg_b1_s1_QLD = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+NSW.WINDGEN+QLD.DEMAND)
VIC_reg_b1_s1_SA = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+SA.DEMAND)
VIC_reg_b1_s1_TAS = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+TAS.DEMAND)
VIC_reg_b1_s1_NSW = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+NSW.DEMAND)
SA_reg_b1_s1_VIC = lm(nem$SA.Price[1:575]~SA.PRICE+SA.DEMAND+SA.WINDGEN+VIC.DEMAND)
TAS_reg_b1_s1_VIC = lm(nem$TAS.Price[1:575]~TAS.PRICE+TAS.DEMAND+TAS.WINDGEN+VIC.DEMAND)

#Batch1 Spec2
QLD_reg_b1_s2_NSW = lm(nem$QLD.Price[1:575]~QLD.PRICE+QLD.DEMAND+nem$QLD.WIND[1:575]+NSW.DEMAND)
NSW_reg_b1_s2_VIC = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+nem$NSW.WIND[1:575]+VIC.DEMAND)
NSW_reg_b1_s2_QLD = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+nem$NSW.WIND[1:575]+QLD.DEMAND)
VIC_reg_b1_s2_SA = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+SA.DEMAND)
VIC_reg_b1_s2_TAS = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+TAS.DEMAND)
VIC_reg_b1_s2_NSW = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+NSW.DEMAND)
SA_reg_b1_s2_VIC = lm(nem$SA.Price[1:575]~SA.PRICE+SA.DEMAND+nem$SA.WIND[1:575]+VIC.DEMAND)
TAS_reg_b1_s2_VIC = lm(nem$TAS.Price[1:575]~TAS.PRICE+TAS.DEMAND+nem$TAS.WIND[1:575]+VIC.DEMAND)

#Batch2 Spec1
QLD_reg_b2_s1_NSW_wd = lm(nem$QLD.Price[576:nrow(nem)]~QLD.PRICE2+QLD.DEMAND2+QLD.WINDGEN2+NSW.DEMAND2)
NSW_reg_b2_s1_VIC_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+NSW.WINDGEN2+VIC.DEMAND2)
NSW_reg_b2_s1_QLD_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+NSW.WINDGEN2+QLD.DEMAND2)
VIC_reg_b2_s1_SA_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+SA.DEMAND2)
VIC_reg_b2_s1_TAS_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+TAS.DEMAND2)
VIC_reg_b2_s1_NSW_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+NSW.DEMAND2)
SA_reg_b2_s1_VIC_wd = lm(nem$SA.Price[576:nrow(nem)]~SA.PRICE2+SA.DEMAND2+SA.WINDGEN2+VIC.DEMAND2)
TAS_reg_b2_s1_VIC_wd = lm(nem$TAS.Price[576:nrow(nem)]~TAS.PRICE2+TAS.DEMAND2+TAS.WINDGEN2+VIC.DEMAND2)

#Batch2 Spec2
QLD_reg_b2_s2_NSW_wd = lm(nem$QLD.Price[576:nrow(nem)]~QLD.PRICE2+QLD.DEMAND2+nem$QLD.WIND[576:nrow(nem)]+NSW.DEMAND2)
NSW_reg_b2_s2_VIC_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+nem$NSW.WIND[576:nrow(nem)]+VIC.DEMAND2)
NSW_reg_b2_s2_QLD_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+nem$NSW.WIND[576:nrow(nem)]+QLD.DEMAND2)
VIC_reg_b2_s2_SA_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+SA.DEMAND2)
VIC_reg_b2_s2_TAS_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+TAS.DEMAND2)
VIC_reg_b2_s2_NSW_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+NSW.DEMAND2)
SA_reg_b2_s2_VIC_wd = lm(nem$SA.Price[576:nrow(nem)]~SA.PRICE2+SA.DEMAND2+nem$SA.WIND[576:nrow(nem)]+VIC.DEMAND2)
TAS_reg_b2_s2_VIC_wd = lm(nem$TAS.Price[576:nrow(nem)]~TAS.PRICE2+TAS.DEMAND2+nem$TAS.WIND[576:nrow(nem)]+VIC.DEMAND2)


##Regerssions with weekday/end dummy
#Batch1 Spec1
QLD_reg_b1_s1_NSW_wd = lm(nem$QLD.Price[1:575]~QLD.PRICE+QLD.DEMAND+QLD.WINDGEN+NSW.DEMAND+dummy.weekday[1:575])
NSW_reg_b1_s1_VIC_wd = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+NSW.WINDGEN+VIC.DEMAND+dummy.weekday[1:575])
NSW_reg_b1_s1_QLD_wd = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+NSW.WINDGEN+QLD.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s1_SA_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+SA.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s1_TAS_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+TAS.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s1_NSW_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+VIC.WINDGEN+NSW.DEMAND+dummy.weekday[1:575])
SA_reg_b1_s1_VIC_wd = lm(nem$SA.Price[1:575]~SA.PRICE+SA.DEMAND+SA.WINDGEN+VIC.DEMAND+dummy.weekday[1:575])
TAS_reg_b1_s1_VIC_wd = lm(nem$TAS.Price[1:575]~TAS.PRICE+TAS.DEMAND+TAS.WINDGEN+VIC.DEMAND+dummy.weekday[1:575])

#Batch1 Spec2
QLD_reg_b1_s2_NSW_wd = lm(nem$QLD.Price[1:575]~QLD.PRICE+QLD.DEMAND+nem$QLD.WIND[1:575]+NSW.DEMAND+dummy.weekday[1:575])
NSW_reg_b1_s2_VIC_wd = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+nem$NSW.WIND[1:575]+VIC.DEMAND+dummy.weekday[1:575])
NSW_reg_b1_s2_QLD_wd = lm(nem$NSW.Price[1:575]~NSW.PRICE+NSW.DEMAND+nem$NSW.WIND[1:575]+QLD.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s2_SA_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+SA.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s2_TAS_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+TAS.DEMAND+dummy.weekday[1:575])
VIC_reg_b1_s2_NSW_wd = lm(nem$VIC.Price[1:575]~VIC.PRICE+VIC.DEMAND+nem$VIC.WIND[1:575]+NSW.DEMAND+dummy.weekday[1:575])
SA_reg_b1_s2_VIC_wd = lm(nem$SA.Price[1:575]~SA.PRICE+SA.DEMAND+nem$SA.WIND[1:575]+VIC.DEMAND+dummy.weekday[1:575])
TAS_reg_b1_s2_VIC_wd = lm(nem$TAS.Price[1:575]~TAS.PRICE+TAS.DEMAND+nem$TAS.WIND[1:575]+VIC.DEMAND+dummy.weekday[1:575])

#Batch2 Spec1
QLD_reg_b2_s1_NSW_wd = lm(nem$QLD.Price[576:nrow(nem)]~QLD.PRICE2+QLD.DEMAND2+QLD.WINDGEN2+NSW.DEMAND2+dummy.weekday[576:nrow(nem)])
NSW_reg_b2_s1_VIC_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+NSW.WINDGEN2+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])
NSW_reg_b2_s1_QLD_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+NSW.WINDGEN2+QLD.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s1_SA_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+SA.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s1_TAS_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+TAS.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s1_NSW_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+VIC.WINDGEN2+NSW.DEMAND2+dummy.weekday[576:nrow(nem)])
SA_reg_b2_s1_VIC_wd = lm(nem$SA.Price[576:nrow(nem)]~SA.PRICE2+SA.DEMAND2+SA.WINDGEN2+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])
TAS_reg_b2_s1_VIC_wd = lm(nem$TAS.Price[576:nrow(nem)]~TAS.PRICE2+TAS.DEMAND2+TAS.WINDGEN2+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])

#Batch2 Spec2
QLD_reg_b2_s2_NSW_wd = lm(nem$QLD.Price[576:nrow(nem)]~QLD.PRICE2+QLD.DEMAND2+nem$QLD.WIND[576:nrow(nem)]+NSW.DEMAND2+dummy.weekday[576:nrow(nem)])
NSW_reg_b2_s2_VIC_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+nem$NSW.WIND[576:nrow(nem)]+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])
NSW_reg_b2_s2_QLD_wd = lm(nem$NSW.Price[576:nrow(nem)]~NSW.PRICE2+NSW.DEMAND2+nem$NSW.WIND[576:nrow(nem)]+QLD.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s2_SA_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+SA.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s2_TAS_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+TAS.DEMAND2+dummy.weekday[576:nrow(nem)])
VIC_reg_b2_s2_NSW_wd = lm(nem$VIC.Price[576:nrow(nem)]~VIC.PRICE2+VIC.DEMAND2+nem$VIC.WIND[576:nrow(nem)]+NSW.DEMAND2+dummy.weekday[576:nrow(nem)])
SA_reg_b2_s2_VIC_wd = lm(nem$SA.Price[576:nrow(nem)]~SA.PRICE2+SA.DEMAND2+nem$SA.WIND[576:nrow(nem)]+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])
TAS_reg_b2_s2_VIC_wd = lm(nem$TAS.Price[576:nrow(nem)]~TAS.PRICE2+TAS.DEMAND2+nem$TAS.WIND[576:nrow(nem)]+VIC.DEMAND2+dummy.weekday[576:nrow(nem)])