#### What is COMER score?

<p align="center">
  <img src="https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/graphical_abstract.png"  title="Graphical abstract" style="max-width: 80%; max-height: 80%;">
</p>

The COVID-19 Methylation Risk (COMER) score is a continuous score that indicates the severity of COVID-19 disease. It is generated by a linear regression equation that uses the methylation beta value of three CpG sites (cg13452062, cg07189579, and cg22652934) as input arguments, with an optional age addition. The equations and predicted thresholds are detailed in below:

Standard equation:

&nbsp; If a patient's COMER score is greater than or equal to 2.4, they will be predicted as Severe.

<p align="center">
  <img src="https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/About_1_COMER_standard.png"  title="Standard COMER score equation" style="max-width: 80%; max-height: 80%;">
</p>

Age adjusted equation:

&nbsp; If a patient's age adjusted COMER score is greater than or equal to 2.46, they will be predicted as Severe.

<p align="center">
  <img src="https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/About_2_COMER_extend.png"  title="Age adjusted COMER score equation" style="max-width: 80%; max-height: 80%;">
</p>

    Where:
        α : age
        β1: beta value of cg13452062
        β2: beta value of cg07189579
        β3: beta value of cg22652934

#### Why is COMER score?

Due to the shortage of healthcare facilities during the global-scale COVID-19 pandemic, prioritizing healthcare resources for patients who are at higher risk of progressing to severe circumstances is critical. To address this problem, a simplified tool was developed to differentiate the severity of these patients based on the whole blood DNA methylation characteristics.

#### How good is the COMER score?

The COMER score is highly specific for SARS-CoV-2 infection compared to Respiratory Syncytial Virus and other respiratory viruses. The tool's performance is consistent across the testing set and independent validation cohort (AUCs are 0.85 and 0.83, respectively), demonstrating a stable model with minimal overfitting and good discrimination between severe and non-severe progressors. The COMER score displays strong correlations with two other commonly used COVID-19 severity clinical scales: the WHO Ordinal Scale and the COVID-19 GRAM risk-score. COMER can predict the outcome of interest early.

<p align="center">
  <img src="https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/About_3_good_COMER.png"  title="How good is COMER score?" style="max-width: 80%; max-height: 80%;">
</p>

The COMER score stands out from other COVID-19 disease prediction tools due to its simplicity and user-friendliness. This innovative tool might bring a fresh perspective to the approach to research and treatment for COVID-19 patients. Unlike other prediction tools that rely on complex models, numerous clinical variables, or subjective predictors that require professional training, the COMER score is a straightforward and routine application tool that can be easily utilized by healthcare professionals. With its ability to accurately predict COVID-19 severity and outcome, the COMER score is a powerful tool in the fight against the pandemic.

The detail report of COMER score: @@@

Data availability: https://zenodo.org/deposit/7846463

Data analysis and visualization source code: https://github.com/TBThien25/COMER_score

COMER score online tool source code: https://github.com/lehieu2307/ShinyAppCovid

#### Members of Team

1. Tran Nguyen Trong Phu [1, 2]
2. Tran Ba Thien [3]
3. Le Van Hieu [4]
4. Le Nhat Thong [5]
5. Tran Thi Thanh Khuong [3]
6. Limothai Umaporn [6, 7]
7. Srisawat Nattachai [6, 7, 8]
8. Luu Phuc Loi [9]

####

_[1] Faculty of Medicine, Chulalongkorn University, Bangkok, Thailand._
_[2] Faculty of Medicine, Can Tho University of Medicine and Pharmacy, Vietnam._
_[3] Stem cell Laboratory, Institute of Food and Biotechnology, Can Tho University, Can Tho City, Vietnam._
_[4] Faculty of Information Technology, University of Sciences, Hue University, Hue city, Vietnam._
_[5] Research Center for Infectious Diseases, International University, Vietnam National University of Ho Chi Minh City, Vietnam._
_[6] Excellence Center for Critical Care Nephrology, King Chulalongkorn Memorial Hospital, Bangkok, Thailand._
_[7] Center of Excellence in Critical Care Nephrology, Faculty of Medicine, Chulalongkorn University, Bangkok, Thailand._
_[8] Academy of Science, Royal Society of Thailand, Bangkok, Thailand._
_[9] Pacific Informatics, Ho Chi Minh city, Vietnam._

 <p align="center">
  <img src="https://raw.githubusercontent.com/lehieu2307/ShinyAppCovid/main/Avatar.png"  title="Authors of COMER score" style="max-width: 100%; max-height: 100%;">
</p>
