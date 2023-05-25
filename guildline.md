The COMER score Shiny app tool provides a simple and user-friendly interface for predicting COVID-19 severity in individuals. The tool includes two prediction tabs: "Multiple Prediction" and "Single Prediction", both of which generate the same COMER score and predicted outcome for the same individual. The main difference between the two tabs is that "Multiple Prediction" allows users to observe the distribution of their entire population based on COMER score, while "Single Prediction" provides a detailed visualization of the value of each CpG site and the corresponding COMER score for the individual in question.
<br>

#### COMER score equations

Standard equation:

**COMER score = 6.9651 - 3.2713×β1 - 6.6951×β2 - 6.3909×β3**

If a patient's COMER score is greater than 2.4, they will be predicted
as Severe.
<br>

Age adjusted equation:

**COMER score = 6.589859 + 0.004134×α - 3.235885×β1 - 6.378156×β2 - 6.233536×β3**

If a patient's age adjusted COMER score is greater than 2.46, they will be predicted as Severe.

    Where:
        α : age
        β1: beta value of cg13452062
        β2: beta value of cg07189579
        β3: beta value of cg22652934

#### Data requirement

COMER score is generated by using methylation beta value of three CpG sites (cg13452062, cg07189579, and cg22652934), with an optional age addition. So, your TSV format data must contain at least 5 columns (sample_ID, age, cg13452062, cg07189579, and cg22652934), with each row representing one sample/individual.

#### Multiple Prediction:

1.  **Import data**
    - Click on the "Browse" button and select your file.
2.  **Start prediction**
    - Click on "Prediction without age" for standard equation or "Prediction with age" for age adjusted equation.
    - Please wait for a while. The waiting time will depend on the size of your data.
3.  **Observe the results**
    - On the lower left, there is an interactive boxplot that allows you to observe the distribution of your data's COMER score. Clicking on a sample point will reveal details of that particular sample. You have the option to display either the Severe or Non-severe predicted samples. The boxplot can be zoomed in and downloaded.
    - On the lower right, you will find a result table that summarizes your samples' COMER scores and predicted outcomes. You can search for a specific individual using the search bar, and the summary table can be copied or downloaded in CSV, Excel, or PDF format.

#### Single Prediction:

1.  **Import data**
    - Click on the "Browse" button and select your file.
2.  **Start prediction**
    - Click on "Prediction".
    - Please wait for a while. The waiting time will depend on the size of your data.
3.  **Choose your predicted individual**
    - Click on the "sample_ID" within the information table to select which individual you want to see more detail about.
4.  **Choose the COMER score equation**
    - Scroll down to see more detail of your selected sample.
    - Click on "Plot without age" for standard equation or "Plot with age" for age adjusted equation.
5.  **Observe the results**
    - Starting from the top, you will see an animated avatar that represents the gender of your sample, along with the sample ID and age. Moving down, on the left-hand side, you will find the methylation beta values of three CpG sites. On the right-hand side, you can see the COMER score equation and how to substitute numbers into the equation.
    - Towards the bottom, there is a dot-histogram with a density curve that depicts the distribution of our 1700 samples based on the COMER score. The x-axis represents the increasing COMER score from left to right, while the y-axis shows the density of samples at that score. The dark green diamond-shaped point represents your selected sample/individual. The further the diamond point moves to the right, the higher the COMER score, indicating that the individual is more likely to become severe.