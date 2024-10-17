# Starlink performance Analysis
Overview:

This project analyzes a dataset containing network performance data for the Starlink satellite internet service. The dataset includes download and upload speeds as well as RTT (Round Trip Time) measurements recorded every two seconds over a 24-hour period. The aim is to classify network performance based on these metrics using various statistical methods.

Dataset:

The dataset consists of 14 variables and 50,368 statistical units. It was collected by Telespazio using a Startrinity terminal to simulate user behavior. The dataset analyzed in this report is from December 21, 2022.

Key Variables:
- Local time: Timestamp of the recorded observation.
- Download bandwidth (bps): Download speed in bits per second.
- Upload bandwidth (bps): Upload speed in bits per second.
- RTT (ping) (ms): Round Trip Time, measuring latency between the ground and satellite.
A- dditional Information:
Ten variables were removed from the dataset, eight of which were missing values, and two were deemed superfluous (download/upload bandwidth ratios).

Methods Applied:

- Variable Cleaning and Renaming: Variables were renamed for better readability, and unnecessary columns were removed.
- Creation of New Variables:
- Speed Thresholds: Categorized as none, up, down, and up_and_down based on threshold values for upload (20 million bps) and download (60 million bps).
- RTT Levels: Classified RTT into four levels: excellent, good, average, and poor.
- Jitter Calculation: Jitter values were calculated and analyzed using boxplots.
- Graphical Analysis: Scatterplots and histograms were used to visualize the relationships between upload/download speeds and RTT.
- Class Balancing: An undersampling method was applied to address class imbalances, resulting in a reduced dataset for further analysis.
- Clustering and Classification:
- Model-based clustering (BIC and ICL methods) was applied to identify optimal component numbers.
- Supervised classification was performed using the EDDA method, achieving a correct classification rate of 92.37% on the undersampled dataset and 97.69% on the full dataset.

Results:

The best classification method was EDDA, both for the undersampled and full datasets. Model-based clustering showed less satisfactory results, with CERs indicating suboptimal allocations.

Tools and Libraries:

R: Used for statistical analysis and visualizations (ggplot, mclust, and mixtools packages).
Conclusion:

EDDA was found to be the most effective classification method for this dataset, offering high accuracy. Other methods, such as model-based clustering, were less effective, especially on undersampled data.
