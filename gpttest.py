import openai
import re

# Set up the OpenAI API client
openai.api_key = "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"

# Define a function to convert a natural language sentence into a Prolog predicate
def nl_to_prolog(nl_sentence):
    # Call the OpenAI GPT API to generate the Prolog predicate
    response = openai.Completion.create(
        engine="davinci",
        prompt=f"convert the natural language sentence '{nl_sentence}' to a Prolog predicate. Specifically, "
               f"I would like the predicate to follow the form 'would_play(X)'",
        temperature=0.5,
        max_tokens=50,
        n=1,
        stop=None,
        timeout=30,
    )

    # Extract the Prolog predicate from the API response
    print(response.choices[0].text.strip())
    prolog_predicate = response.choices[0].text.strip()

    # Clean up the predicate string by removing trailing punctuation and other characters
    #prolog_predicate = re.sub(r'[^\w\s,()]', '', prolog_predicate)

    return prolog_predicate

# Example usage of the function
nl_sentence = "I want to play someone strong"
prolog_predicate = nl_to_prolog(nl_sentence)
print(prolog_predicate)
