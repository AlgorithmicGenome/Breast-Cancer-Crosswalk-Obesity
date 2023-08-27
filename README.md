# Breast-Cancer-Crosswalk-Obesity
# Data Sources:
Breast Cancer Anthropometric Data with Machine Learning Algorithms.
The anthropometric data was collected peripheral venous blood vials at the laboratory of physiology of the faculty of medicine at the university of Coimbra in 2018
[Patricio, 2018] Patrício, M., Pereira, J., Crisóstomo, J., Matafome, P., Gomes, M., Seiça, R., & Caramelo, F. (2018). Using Resistin, glucose, age and BMI to predict the presence of breast cancer. BMC Cancer, 18(1)
https://bmccancer.biomedcentral.com/articles/10.1186/s12885-017-3877-1
Crisóstomo, J., Matafome, P., Santos-Silva, D. et al. Hyperresistinemia and metabolic dysregulation: a risky crosstalk in obese breast cancer. Endocrine 53, 433–442 (2016)
https://link.springer.com/article/10.1007/s12020-016-0893-x

# Background Information:
The data was donated to the center for machine learning and intelligent systems on March 3rd, 2018
https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra#
This dataset is publicly available for research. The details are described in [Patricio, 2018]
There are 10 quantitative predictors and a binary dependent variable, indicating whether there is breast cancer or not
Quantitative Attributes: Age (years), BMI (kg/m2), Glucose (mg/dL), Insulin (µU/mL), HOMA, Leptin (ng/mL), Adiponectin (µg/mL), Resistin (ng/mL), MCP-1(pg/dL) Labels: 1=Healthy controls 2=Patients

# Methods:
# Biostatistics
Hypothesis testing #1
H0: Glucose, HOMA, and Resistin are three significant contributing factors in biomarkers to breast cancer
H1: Glucose, HOMA, and Resistin are not the only significant contributing factors in biomarkers to breast cancer
Hypothesis Testing #2
H0: Data comes from a normal distribution
H1: Data does not come from a normal distribution
Univariate analysis of categorical and quantitative variables
Normality Testing
Histogram
QQ-plot
Shapiro-Wilk normality
Correlation Matrix
ANOVA Analysis

# Machine learning models on significant variables from ANOVA analysis
Split Train Dataset
Linear Discriminant Analysis
Classification and Regression Trees (CART)
Decision Trees
Random Forest
Neural Net
Support Vector machine





