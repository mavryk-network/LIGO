import React from 'react';
import ReactMarkdown from 'react-markdown';
import Layout from '@theme/Layout';
import Highlight, { defaultProps } from "prism-react-renderer";
import defaultTheme from 'prism-react-renderer/themes/palenight';
import Prism from '../../core/PrismLigoSyntaxes'

// TODO : Add light / dark theme instead of default one
// TODO : Make Ligo syntaxes highlighting work in Markdown code block
// TODO : Add filtering-by-tag function

const QUESTIONS = [
{
  question: "How do you write bold text ?",
  tags: ["bold", "question"],
  answer: `
You just use the markdown syntax **like this** !
`
},
{
  question: "Can you also write lists ?",
  tags: ["list", "question"],
  answer: `
Yup, here it is some quoted text, and a __list__ :

> A block quote with ~strikethrough~ and a URL: https://reactjs.org.

* Lists
* [ ] todo
* [x] done
`
},
{
  question: "How to embed code ?",
  tags: ["code", "question"],
  answer: `

Using Markdown syntax with three '~' :
~~~js
console.log("Some code !");
~~~

Here is a C example :

~~~c++
#include <stdio.h>

int main() {
  return 0;
}
~~~

You can also include *Ligo* code snippets :

~~~cameligo
let main (action, store : parameter * storage) : return =
 ([] : operation list),    // No operations
 (match action with
   Increment (n) -> add (store, n)
 | Decrement (n) -> sub (store, n)
 | Reset         -> 0)
(* Tests for main access point *)
let test_initial_storage =
 let initial_storage = 42 in
 let (taddr, _, _) = Test.originate main initial_storage 0tez in
 assert (Test.get_storage taddr = initial_storage)
~~~
`
}
]


const Question = ({question, answer, tags}) => (
  <div className="qa">
    <div className="question">{question}</div>
    <div className="tags">Tags : {tags.join(', ')}</div>
    <div className="answer">
      <ReactMarkdown
        children={answer}
        // Specifying the code() function in the below component attribute
        // enable us to specify how to render code blocks in Markdown, written between '~~~'
        components={{
          code({node, inline, className, children, ...props}) {
            // When typing markdown ~~~my_language ... ~~~, the className is "language-my_language"
            // So we use a regexp to remove the "language-" prefix here
            const match = /language-(\w+)/.exec(className || '')
            const language = match[1]
            const code = children[0]
            return (
              <Highlight {...defaultProps} code={code} language={language} style={defaultTheme}>
                {/* This function was taken directly from the prism-react-renderer Github page README */}
                {({ className, style, tokens, getLineProps, getTokenProps }) => (
                  <pre className={className} style={style}>
                    {tokens.map((line, i) => (
                      <div {...getLineProps({ line, key: i })}>
                        {line.map((token, key) => (
                          <span {...getTokenProps({ token, key })} />
                        ))}
                      </div>
                    ))}
                  </pre>
                )}
              </Highlight>
            )
          }
        }}
      /> {/* End of <ReactMarkdown> */}
    </div>
  </div>
);

export default props => {
  return (
    <Layout title="Faq">
      <div
        id="faqPage"
        style={{
          display: 'flex',
          justifyContent: 'stretch',
          width: '95vw',
          fontSize: '20px',
          flexDirection: 'column'
        }}>
        <div className="title">Frequently Asked Questions</div>
        <div className="body">
          {QUESTIONS.map(entry =>
            <Question question={entry.question} answer={entry.answer} tags={entry.tags} />
          )}
        </div>
      </div>
    </Layout>
  );
}
