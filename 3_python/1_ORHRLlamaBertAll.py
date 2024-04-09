import pyarrow.feather as feather
import pandas as pd
from ctransformers import AutoModelForCausalLM, AutoTokenizer, AutoConfig
import time
import os
import torch
from transformers import AutoTokenizer, AutoModelForQuestionAnswering, pipeline

projectName = "stridorIahMortality"

# Set environment variables
os.environ["TOKENIZERS_PARALLELISM"] = "false"  # Disable download progress
os.environ["HF_HUB_DISABLE_SYMLINKS_WARNING"] = "1"  # Disable symlink warning


df10 = feather.read_feather('C:/Users/Federico/Desktop/8_Abstracts/3_forPython/1_toPythonDB/1_ORHRExposureForLlama/ORHRRRLLAMAExposure' + projectName + '.feather')
# df10 = df.iloc[30:31]

# Create 'answer' columns for exposure, outcome, participants, and reference

df10['ansExposureLlama'] = ""
df10['timeExecutionExp'] = ""


# Set gpu_layers to the number of layers to offload to GPU. Set to 0 if no GPU acceleration is available on your system.
model_path = 'C:/Users/Federico/Desktop/8_Abstracts/4_Python/llmModels/3_TheBloke_Marcoroni_7b_GGUF'

config = AutoConfig.from_pretrained(model_path)
config.max_seq_len = 1000

llm = AutoModelForCausalLM.from_pretrained(model_path,
                                           model_file="marcoroni-7b.Q4_K_M.gguf",
                                           model_type="llama",
                                           gpu_layers=0)

save_every_n_iterations = 20

for index, row in df10.iterrows():
    start_time = time.time()

    quest_exposure = row['contextLLAMAExposure']


    # Set the 'answer' columns for the current row

    df10.at[index, 'ansExposureLlama'] = llm(quest_exposure)


    end_time = time.time()
    execution_time = end_time - start_time
    df10.at[index, 'timeExecutionExp'] = str(execution_time)

    # Save the output to a CSV file every 1 loop iterations
    if index % save_every_n_iterations == 0:

     # Save the DataFrame as a Feather file (you can adjust the file name as needed)
       feather.write_feather(df10,
                              'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/1_exposure/outORHRLlamaExposurePartial' + projectName + '.feather')


# Save the DataFrame as a Feather file (you can adjust the file name as needed)
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/1_exposure/outORHRLlamaExposureFinal' + projectName + '.feather')


## treat exposure with Roberta

# Load tokenizer and model
model_path = 'C:/Users/Federico/Desktop/8_Abstracts/4_Python/llmModels/topModels/questionAnswering/roberta_base_squad2'
tokenizer = AutoTokenizer.from_pretrained(model_path)
model = AutoModelForQuestionAnswering.from_pretrained(model_path)

qa_model = pipeline("question-answering", model=model, tokenizer=tokenizer)

# Create 'answer' columns for exposure, outcome, participants, and reference
df10['ansExposureRoby'] = ""

df10['questExposure'] = "What is the main exposure or risk factor?"

# Iterate over rows
save_every_n_iterations = 20
for index, row in df10.iterrows():
    # Get the context from the row and strip whitespace
    contextExposure = row['ansExposureLlama'].strip()

    # Check if the context is empty or contains only whitespace
    if not contextExposure:
        contextExposure = "empty"


    # Get the questions for each column
    quest_exposure = row['questExposure']

    # Answer questions using the QA model
    qa_result_exposure = qa_model(question=quest_exposure, context=contextExposure)


    # Set the 'answer' columns for the current row
    df10.at[index, 'ansExposureRoby'] = qa_result_exposure["answer"]


    # Save the output to a CSV file every 10 loop iterations
    if index % save_every_n_iterations == 0:
        feather.write_feather(df10,
                              'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/1_exposure/outORHRLlamaBertExposurePartial' + projectName + '.feather')

# Save the final output to a CSV file
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/1_exposure/outORHRLlamaBertExposureFinal' + projectName + '.feather')


## find outcome with Llama, considering previous found exposure

# df10['questOutcomeLlama'] = 'CONTEXT: ' + df10['contextORHR'] + 'ANALYSIS: We are searching the event, disease or medical condition for which the odds or risk is associated with or predicted by the ' + df10['measureType'] + ' :' + df10['result'] + ' . The event, disease or medical condition that we are looking for is '
df10['questOutcomeLlama'] = df10['contextORHR'] + 'Considering the previous text, answer the question: The odds or risk for which event, disease or medical condition are associated with or predicted by ' + df10['ansExposureRoby'] + 'in the ' + df10['measureType'] + ' :' + df10['result'] + ' ?'
df10['ansOutcomeLlama'] = ""
df10['timeExecutionOut'] = ""

# Loop Outcome Llama

for index, row in df10.iterrows():
    start_time = time.time()

    quest_outcomeLlama = row['questOutcomeLlama']


    # Set the 'answer' columns for the current row

    df10.at[index, 'ansOutcomeLlama'] = llm(quest_outcomeLlama)


    end_time = time.time()
    execution_time = end_time - start_time
    df10.at[index, 'timeExecutionOut'] = str(execution_time)

    # Save the output to a CSV file every 1 loop iterations
    if index % save_every_n_iterations == 0:

     # Save the DataFrame as a Feather file (you can adjust the file name as needed)
       feather.write_feather(df10,
                              'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome/outORHRLlamaOutcomePartial' + projectName + '.feather')


# Save the DataFrame as a Feather file (you can adjust the file name as needed)
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome/outORHRLlamaOutcomeFinal' + projectName + '.feather')


# treat outcome with Roby

df10['questOutcomeRoby'] = "What is the main outcome or event?"
df10['ansOutcomeRoby'] = ""

# Iterate over rows
save_every_n_iterations = 20
for index, row in df10.iterrows():
    # Get the context from the row and strip whitespace
    contextOutcome = row['ansOutcomeLlama'].strip()

    # Check if the context is empty or contains only whitespace
    if not contextOutcome:
        contextOutcome = "empty"


    # Get the questions for each column
    quest_outcome = row['questOutcomeRoby']

    # Answer questions using the QA model
    qa_result_outcome = qa_model(question=quest_outcome, context=contextOutcome)


    # Set the 'answer' columns for the current row
    df10.at[index, 'ansOutcomeRoby'] = qa_result_outcome["answer"]


    # Save the output to a CSV file every 10 loop iterations
    if index % save_every_n_iterations == 0:
        feather.write_feather(df10,
                              'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome/outORHRLlamaBertOutcomePartial' + projectName + '.feather')

# Save the final output to a CSV file
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/1_ORHRLlamaBertTreated/2_outcome/outORHRLlamaBertOutcomeFinal' + projectName + '.feather')
