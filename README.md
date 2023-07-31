# AI-assisted Medical Self-Diagnosis Simulation

This project evaluates the performance of GPT-3.5 Turbo, an advanced AI language model developed by OpenAI, in simulating the process of self-diagnosing medical conditions using online resources.

## Objective

With a growing number of people resorting to online platforms like Google and WebMD for self-diagnosis, we aim to simulate this process and assess how an AI model like GPT-3.5 Turbo compares with human raters.

## Methodology

We generate a representative patient dataset that includes disease prevalence, patient demographics, and a list of symptoms. The dataset is designed to mimic real-world scenarios where patients may not report all relevant symptoms or may report irrelevant ones. Using this dataset, GPT-3.5 Turbo is tasked with generating potential diagnoses. 

The diagnoses provided by GPT-3.5 Turbo are then compared with those obtained from human raters using the same online resources. This comparison provides insight into the capabilities and limitations of AI in the context of self-diagnosis.

## Results

The results of this study contribute to our understanding of the potential applications and limitations of AI in healthcare, especially in the context of self-diagnosis. The findings could guide future developments in AI-assisted healthcare and inform policy discussions about the role of AI in health-related decision-making.

## How to Use

To replicate this study, you can use the provided R and python scripts and the simulated patient dataset. The script outlines the process of interacting with the GPT-3.5 Turbo model using OpenAI's API. Please replace the API key placeholder with your own API key to run the script.
