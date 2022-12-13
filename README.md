# Purpose

This repository contains the scripts for generating the figures in the EEGManyPipelines (www.eegmanypipelines.org) position paper. 

In the paper we describe the motivation for EEGManyPipelines and present an overview of the analysts who participated in analysing the EEG dataset. For more information, please refer to:

> **Trübutschek, D. et al. *EEGManyPipelines: A large-scale, grass-root multi-analyst study of EEG analysis practices in the wild.* (2022). doi:10.31222/osf.io/jq342**

![Image](https://pbs.twimg.com/media/Fj2MlynWYAQ8XbT?format=png&name=small "Figure")

## In this folder

* ``import_data.R``: import the data tables with the analysts' responses, rearragne data, and recode variables for plotting.
* ``plotEEGExpertise.R``: plot self-reported experience, academic level, etc. (panel C).
* ``plotGender.R``: plot the gender distribution of the EEGManyPipes analysts (panel B).
* ``plot_pubmed_data_and_represent.R``: plot distribution of the EEGManyPipes analysts compared to PubMed data (panel E).
* ``representativeData.R``: plot the EMP sample data in comparison to the pubmed data (alternative panel E).
* ``save_pubmed_oneCSV.R``: import the data tables with the analysts' responses, rearragne data
* ``worldMap.R``: plot the world map for the EEGManyPipes analysts origin (panel D).
