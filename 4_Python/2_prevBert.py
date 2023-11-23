import pandas as pd
import pyarrow.feather as feather
import os
import torch
from transformers import AutoTokenizer, AutoModelForQuestionAnswering, pipeline


projectName = "stridorIahMortality"

# Set environment variables
os.environ["TOKENIZERS_PARALLELISM"] = "false"  # Disable download progress
os.environ["HF_HUB_DISABLE_SYMLINKS_WARNING"] = "1"  # Disable symlink warning

# Load tokenizer and model
model_path = 'C:/Users/Federico/Desktop/8_Abstracts/4_Python/llmModels/topModels/questionAnswering/roberta_base_squad2'
tokenizer = AutoTokenizer.from_pretrained(model_path)
model = AutoModelForQuestionAnswering.from_pretrained(model_path)

qa_model = pipeline("question-answering", model=model, tokenizer=tokenizer)

# Import data with pandas
df10 = feather.read_feather('C:/Users/Federico/Desktop/8_Abstracts/3_forPython/1_toPythonDB/2_PrevalenceForBert/prevBert' + projectName + '.feather')
# df10 = df.iloc[10:20]


# Create 'answer' columns for exposure, outcome, participants, and reference

df10['ansFreq'] = ""
df10['ansGroup'] = ""

# Iterate over rows
save_every_n_iterations = 20
for index, row in df10.iterrows():
    # Get the context from the row and strip whitespace

    contextFreq = row['BertShortContext'].strip()

    # Check if the context is empty or contains only whitespace

    if not contextFreq:
        contextFreq = "empty"

    # Get the questions for each column

    quest_freq = row['questBertPercExposure']


    # Answer questions using the QA model

    qa_result_freq = qa_model(question=quest_freq, context=contextFreq)


    # Set the 'answer' columns for the current row

    df10.at[index, 'ansFreq'] = qa_result_freq["answer"]


df10['questGroupBert'] = df10['keywordFinal'] + ' corresponds to the percentage of ' + df10['ansFreq'] + ' in a group defined by which characterisitc? do not mention ' + df10['ansFreq'] + '.'

# Iterate over rows
save_every_n_iterations = 20
for index, row in df10.iterrows():
    # Get the context from the row and strip whitespace

    contextFreq = row['BertShortContext'].strip()

    # Check if the context is empty or contains only whitespace

    if not contextFreq:
        contextFreq = "empty"

    # Get the questions for each column

    quest_freq = row['questGroupBert']


    # Answer questions using the QA model

    qa_result_freq = qa_model(question=quest_freq, context=contextFreq)


    # Set the 'answer' columns for the current row

    df10.at[index, 'ansGroup'] = qa_result_freq["answer"]


# Save the final output to a CSV file
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/2_prevBert/outPrevBertFinal' + projectName + '.feather')

