# <span style="color:#22223b"> Bellabeat Case Study  </span>
![](https://th.bing.com/th/id/R.a1e9c2ad22f7cfab0c59fbd1cd9fc2fc?rik=17y7jjy%2bo3NWsg&riu=http%3a%2f%2fdizajn.hr%2fwp-content%2fuploads%2f2017%2f01%2fad_hdd-900x600.jpeg&ehk=MvAFG1rywDFxu0txBwxQCcmM6ArCfUcWrH0MGY%2b4A9U%3d&risl=&pid=ImgRaw&r=0)


# <span style="color:#22223b"> 1. Introduction </span> <a id="summary_1"></a>
Bellabeat is a high-tech manufacturer of health-focused products for women. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. I have been asked to focus on one of Bellabeat’s products and analyze smart device data to gain insight into how consumers are using their smart devices. The insights discovered will then help guide marketing strategy for the company.
My report will include the following deliverables:
1. A clear summary of the business task
2. A description of all data sources used
3. Documentation of any cleaning or manipulation of data
4. A summary of your analysis
5. Supporting visualizations and key findings
6. Your top high-level content recommendations based on your analysis

# <span style="color:#22223b"> 2. Ask Phase </span> <a id="ask_phase"></a>
Sršen has asked to analyze smart device usage data in order to gain insight into how consumers use non-Bellabeat smart devices. She then wants one Bellabeat product to apply these insights to in your presentation. These questions guides the analysis:
1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?
 
#### <span style="color:#22223b"> 2.1 Summary of Business Task </span> <a id="business_task_2.1"></a>  
Identify trends in how consumers use non-Bellabeat smart devices to apply insights into Bellabeat’s marketing strategy.

# <span style="color:#22223b"> 3. Prepare Phase </span> <a id="prepare_3"></a>
The prepare phase ensures you have all of the data you need for your analysis and that you have credible, useful data. Here, I used the business task as a guide to decide which data from the dataset is relevant to my analysis.

#### <span style="color:#22223b"> 3.1 Data Sources Used and Description</span> <a id="data_used_3.1"></a>
Sršen encourages to use public data that explores smart device users’ daily habits. She points to a specific data set: [fitbit_dataset](http://https://www.kaggle.com/arashnic/fitbit) .
This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.
This dataset was generated by respondents to a distributed survey via Amazon Mechanical Turk between 03.12.2016-05.12.2016. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. Individual reports can be parsed by export session ID (column A) or timestamp (column B). Variation between output represents use of different types of Fitbit trackers and individual tracking behaviors / preferences.

#### <span style="color:#22223b"> 3.2 Data Lincensing, Credibility and Accessibility </span> <a id="data_used_3.2"></a>
This data is confirmed to be lincensed under an open lincense. This data is openly accessible, exploitable, editable and shared by anyone for any purpose.

#### <span style="color:#22223b"> 3.3 Data Integrity </span> <a id="data_used_3.3"></a>
Although this data has the potential of answering our business question, it does have some limitations in the sense that the sample size is quite small (30 users) and not representative of the poulation of smart device users.

# <span style="color:#22223b"> 4. Process Phase </span> <a id="process_4"></a>
Now I know the data is credible and relevant to the business problem, I need to clean it so that my analysis is error-free. For this analysis, I will be using R due to its flexibility, the size of data to be analysed and also to be able to create data visualization to share my results with stakeholders. 

#### <span style="color:#22223b"> 4.1 Loading Packages </span> <a id="process_4.1"></a>
To proceed with this, I need to load some relevant R packages to begin wrangling and analysing:  

* library(tidyverse)  
* library(janitor)  
* library(lubridate)  
* library(patchwork)  


# <span style="color:#22223b"> 5. Analyse and Share Phase </span> <a id="analyse_5"></a>
#### <span style="color:#22223b"> 4.1 Loading Packages </span> <a ></a>
Bellabeat is a successful small company that has the potential to become a larger player in the global smart device market.

In order to have more a more robust recommedation to enable Bellabeat become a larger player, I first recommend that exixting smart devices should be used to collect more detailed data of customers as regard age and demographics. Furthermore, online surveys is another way Bellabeat can uncover more details about their customers as well as their preferences. 

However, from the above analysis, I reccommend the following for the **Bellabeat Time product**;

|Recommendation| Description |
|---| ---|
|1. Sedentary Minutes Check | The analysis shows that users spend about 11hrs each day immobile. Users are most likely unaware of this for serveral reasons. A notification can be sent to users if they have been at the same time for too long to get up and take some steps or stretch to get blood flowing. 
|2. Sleep time| Users are not getting the recommended 8hrs of sleep daily. I recommend a notification to remind users when its bed time and time to get up. 
|3. Goals | I consider it a good idea if the Bellabeat Time product can give users targets at the begining of day say, "Hello Mark, do you think you can burn 1000 calories today? well, let's see you try!."|4. Benefits| Most people don't like being told what to do more especially when its from a gadget. I recommend that periodically health fun facts are shown to users to remind them how important lower sedntary minutes, taking more steps, among other health tips are for the body.|
|4. Featureas | To encourage more usage days, the Time Product can be made to appear more fashionable and elegant to go with a variety of attaires.






























