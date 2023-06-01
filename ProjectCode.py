import openai
import pytholog as pl

# Set up the OpenAI API client
openai.api_key = "API Key here"

# Define a function to convert a natural language sentence into a Prolog predicate
def nl_to_prolog(prompt):
    # Call the OpenAI GPT API to generate the Prolog predicate
    context = '''

    playstyle(strong / skillful / supportive / magical) race(dwarf / human / elf / halforc) class(fighter / bard / cleric / wizard)
    
    "query" only show up in the questions for the value the question is asking for.
    *start*
    There is a hero named Zenc who is a human fighter. ### name(zenc), race(human), class(fighter) 
    Eve is a magical elf character that is a wizard. ### name(eve), race(elf), class(wizard), playstyle(magical)
    Korvus is a supportive halforc character. ### name(korvus), playstyle(supportive), race(halforc)
    The hero Fela is a dwarf bard. ### name(fela), race(dwarf), class(bard)
    What character is a dwarf? ### race(dwarf), name(query)
    I want to play a strong character. ### playstyle(strong), name(query)
    What style is Korvus? ### name(korvus), playstyle(query)
    What race is Zenc? ### name(zenc), race(query)
    I would like to play a character that is a dwarf bard. ### name(query), race(dwarf), class(bard)
    I want to play a character. ### name(query)
    Can you recommend a character? ### name(query)
    '''
    prediction = openai.Completion.create(
        model="text-davinci-003",
        prompt=context + prompt)

    return prediction['choices'][0]['text']

def sanitize(pred):
    pred1, cooked1 = sanitizehelp(pred, "name", ["zenc", "eve", "korvus", "fela"])
    pred2, cooked2 = sanitizehelp(pred1, "playstyle", ["strong", "skillful", "supportive", "magical"])
    pred3, cooked3 = sanitizehelp(pred2, "race", ["dwarf", "human", "elf", "halforc"])
    pred4, cooked4 = sanitizehelp(pred3, "class", ["fighter", "bard", "cleric", "wizard"])
    c = cooked1 + cooked2 + cooked3 + cooked4
    return c

def sanitizehelp(pred1, first, second):
    pred = pred1
    cooked = []
    for i in range(pred.count(first)):
        pred.replace(first, '', 1)
        if pred.count(second[0]) > 0:
            cooked.append(first + "(" + second[0] +")")
            pred.replace(second[0], '', 1)
        elif pred.count(second[1]) > 0:
            cooked.append(first + "(" + second[1] +")")
            pred.replace(second[1], '', 1)
        elif pred.count(second[2]) > 0:
            cooked.append(first + "(" + second[2] +")")
            pred.replace(second[2], '', 1)
        elif pred.count(second[3]) > 0:
            cooked.append(first + "(" + second[3] +")")
            pred.replace(second[3], '', 1)
        elif pred.count("query") > 0:
            cooked.append(first + "(query)")
            pred.replace("query", '', 1)
    return pred, cooked

def speak():
    # Call the OpenAI GPT API to generate the Prolog predicate
    context = '''

        Please ask me either what playstyle I want, what race I want, or what class I want. 
        '''
    prediction = openai.Completion.create(
        model="text-davinci-003",
        prompt=context)

    print(prediction['choices'][0]['text'])

#prompt = "Could you recommend a character that is strong?"
cont = True
kb = ["solve(zenc) :- name(query), playstyle(strong)",
      "solve(zenc) :- name(query), class(fighter)",
      "solve(zenc) :- name(query), race(human)",
      "solve(eve) :- name(query), playstyle(magical)",
      "solve(eve) :- name(query), class(wizard)",
      "solve(eve) :- name(query), race(elf)",
      "solve(korvus) :- name(query), playstyle(supportive)",
      "solve(korvus) :- name(query), class(cleric)",
      "solve(korvus) :- name(query), race(halforc)",
      "solve(fela) :- name(query), playstyle(skillful)",
      "solve(fela) :- name(query), class(bard)",
      "solve(fela) :- name(query), race(dwarf)"]
print("Hello! How can I help you?")
while(cont):
    # Prolog translates user input to predicates
    prompt = input()
    prolog_predicate = nl_to_prolog(prompt)
    # GPT predicates sanitized
    preds = sanitize(prolog_predicate)
    print("Predicates: ")
    print(preds)
    new_kb = pl.KnowledgeBase("DnD")
    new_kb(preds + kb)
    # Get answer
    solve = new_kb.query(pl.Expr("solve(X)"))
    if solve[0] == 'No':
        # GPT uses answer set to generate response to user
        speak()
    else:
        ans = solve[0]['X']
        print("You should play " + ans.capitalize() + ".")
        cont = False
    # User responds
