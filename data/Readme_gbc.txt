NAME: 
German Breast Cancer Study Data (gbc.txt)
(gbc_mort.txt contains the subset of mortality data)


SIZE: 
686 Observations, 11 Variables

SOURCE: 
German Breast Cancer Study 

REFERENCE:
Hosmer, D.W. and Lemeshow, S. and May, S. (2008) 
Applied Survival Analysis: Regression Modeling of Time to Event Data: 
Second Edition, John Wiley and Sons Inc., New York, NY

Description of the Study:
Cancer clinical trials are a rich source for examples of applications 
of methods for the analysis of time to event.  Willi Sauerbrei and Patrick 
Royston have graciously provided us with data obtained from the German Breast 
Cancer Study Group, which they used to illustrate methods for building 
prognostic models (Sauerbrei and Royston, 1999).  In the main study, 
a total of 720 patients with primary node positive breast cancer were 
recruited between July 1984, and December 1989, (see Schmoor, Olschweski 
and Schumacher M. 1996 and Schumacher et al. (1994)). 


List of variables:

Variable	Name		     Description                   Codes/Values
************************************************************************************
1               id                   Study ID                      1 - 686
2		time		     Time to Recurrence/
					death/censoring		   Months
3		status		     Event type			   0 = censoring
								   1 = cancer relapse
								   2 = death
4               hormone              Hormone Therapy               1 = No, 2 = Yes
5               age                  Age at Diagnosis              Years
6               meno               Menopausal Status              1 = No, 2 = Yes
7               size                 Tumor Size                    mm
8               grade                Tumor Grade                   1 - 3
9               nodes                Number of Nodes               1 - 51
                                       involved
10              prog	            Number of Progesterone        1 - 2380
                                       Receptors
11              estrg	           Number of Estrogen       	  1 - 1144
                                       Receptors
