import pyarrow.feather as feather
import pandas as pd
from ctransformers import AutoModelForCausalLM, AutoTokenizer, AutoConfig
from transformers import AutoTokenizer, AutoModelForQuestionAnswering, pipeline

projectName = "stridorIahMortality"


# df = pd.read_csv('C:/Users/Federico/Desktop/8_Abstracts/3_forPython/1_toPythonDB/ORHRRRLLAMA' + projectName + '.csv',
#                   encoding='unicode_escape')

df10 = feather.read_feather('C:/Users/Federico/Desktop/8_Abstracts/3_forPython/1_toPythonDB/3_PopDataForBert/participants' + projectName + '.feather')
# df10 = df.iloc[10:11]

# Create 'answer' columns

df10['ansParticipants'] = ""
df10['ansPopDisease'] = ""
df10['ansComorbidity'] = ""
df10['ansAgeNumeric'] = ""
df10['ansWomen'] = ""
df10['ansMen'] = ""

# Load tokenizer and model
model_path = 'C:/Users/Federico/Desktop/8_Abstracts/4_Python/llmModels/topModels/questionAnswering/roberta_base_squad2'
tokenizer = AutoTokenizer.from_pretrained(model_path)
model = AutoModelForQuestionAnswering.from_pretrained(model_path)

# create QA function
qa_model = pipeline("question-answering", model=model, tokenizer=tokenizer)


# Iterate over rows
save_every_n_iterations = 20
for index, row in df10.iterrows():
    # Get the context from the row and strip whitespace
    contextPop = row['ABSTRACT'].strip()

    # Check if the context is empty or contains only whitespace
    if not contextPop:
        contextPop = "empty"


    # Get the questions for each column
    quest_Participants = row['questParticipants']
    quest_PopDisease = row['questPopDisease']
    quest_Comorbidity = row['questComorbidity']
    quest_AgeNumeric = row['questAgeNumeric']
    quest_Women = row['questWomen']
    quest_Men = row['questMen']

    # Answer questions using the QA model
    qa_result_participants = qa_model(question=quest_Participants, context=contextPop)
    qa_result_PopDisease = qa_model(question=quest_PopDisease, context=contextPop)
    qa_result_Comorbidity = qa_model(question=quest_Comorbidity, context=contextPop)
    qa_result_AgeNumeric = qa_model(question=quest_AgeNumeric, context=contextPop)
    qa_result_women = qa_model(question=quest_Women, context=contextPop)
    qa_result_men = qa_model(question=quest_Men, context=contextPop)


    # Set the 'answer' columns for the current row
    df10.at[index, 'ansParticipants'] = qa_result_participants["answer"]

    df10.at[index, 'ansPopDisease'] = qa_result_PopDisease["answer"]
    df10.at[index, 'ansComorbidity'] = qa_result_Comorbidity["answer"]
    df10.at[index, 'ansAgeNumeric'] = qa_result_AgeNumeric["answer"]
    df10.at[index, 'ansWomen'] = qa_result_women["answer"]
    df10.at[index, 'ansMen'] = qa_result_men["answer"]




# Save the final output to a CSV file
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/3_popLlamaBertTreatedDB/popBertFinal' + projectName + '.feather')



df10['ansAgeClass'] = ""
df10['ansSex'] = ""

'''
# Set gpu_layers to the number of layers to offload to GPU. Set to 0 if no GPU acceleration is available on your system.
model_path2 = 'C:/Users/Federico/Desktop/8_Abstracts/4_Python/llmModels/3_TheBloke_Marcoroni_7b_GGUF'

config = AutoConfig.from_pretrained(model_path)
config.max_seq_len = 1000

llm = AutoModelForCausalLM.from_pretrained(model_path2,
                                           model_file="marcoroni-7b.Q4_K_M.gguf",
                                           model_type="llama",
                                           gpu_layers=0)

save_every_n_iterations = 20


for index, row in df10.iterrows():

    quest_AgeClass = row['questAgeClass']
    quest_Sex = row['questSex']

    # Set the 'answer' columns for the current row

    df10.at[index, 'ansAgeClass'] = llm(quest_AgeClass)
    df10.at[index, 'ansSex'] = llm(quest_Sex)

    # Save the output to a CSV file every 1 loop iterations
    if index % save_every_n_iterations == 0:
        # Save the DataFrame as a Feather file (you can adjust the file name as needed)
        feather.write_feather(df10,
                              'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/3_popLlamaBertTreatedDB/popLlamaBertPartial' + projectName + '.feather')

# Save the DataFrame as a Feather file (you can adjust the file name as needed)
feather.write_feather(df10,
                      'C:/Users/Federico/Desktop/8_Abstracts/4_Python/3_LlamaCodes/3_popLlamaBertTreatedDB/popLlamaBertFinal' + projectName + '.feather')

'''