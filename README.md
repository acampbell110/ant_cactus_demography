# ant_partner_transitions_demography

This repo contains all the data and code necessary to reproduce the analyses of the Ant Partner Cholla Demography Project.

\section*{In the Data Analysis Folder:}
This folder includes all R scripts which take the raw data and filter it through the models. It also contains all models for the vital rates. 
\begin{itemize}
\item The script Setup_Data.R reads in the original data set and produces a "cleaned" data script.
\item The script Cholla_Analysis_Vital_Rates.R includes all setup and running of each STAN model. It also produces csv files of each model. It downloads the "cleaned" data script and organizes it into smaller data sets to feed into a large STAN model. The STAN model is run and this code produces new csvs which contain the outputs of each STAN model.
\item The script One_Species_Vacant.R includes all setup and running of the STAN model for the IPM model which includes only one species of ant and the option of vacancy. This script downloads data from the "cleaned" data. It produces  a csv which contains the outputs of the STAN model. 
\item The script Post_Repro_Data_Analysis.R creates the figures which compare simulated data from the STAN models to the real data to ensure the models are accurately predicting the data. 
\end{itemize}

\subsection*{Data}
These are the csvs read in to the models and for analysis. 
\begin{itemize}
\item FruitSurvival.csv contains information fed into the fruit survival models. 
\item Germination.csv contains information fed into the germination models. 
\item JO_furit_data_final_dropplant0.csv contains information fed into the seeds per fruit model.
\item PrecensusSurvival.csv contains information fed into the precensus survival model. 
\item cholla_demography_20042019.csv is the original data collected for reproduction, growth and survival. 
\item cholla_demography_20042019_cleaned.csv is produced by the Setup_Data.R script. 
\item seed_counts.csv contains information fed into the seeds per fruit model. 
\end{itemize}

\subsection*{STAN Models Folder}
\begin{itemize}
\item flower_mix_ant_trunc.stan
\item flower_mix_ant.stan
\item fruit_surv.stan
\item germ_yr1.stan
\item germ_yr2.stan
\item grow_mix_ant.stan
\end{itemize}


\section*{Figures}
This folder includes figures which visualize the outcomes of models created int the STAN models folder. 

\section*{IPM Practice}
This folder includes all R scripts which create and carry out IPM analysis. 