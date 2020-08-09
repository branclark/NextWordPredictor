Capstone Presentation - Text Predictor Web App
========================================================
author: Brandon Clark
date: 8/9/2020
autosize: true


Use Application
========================================================
![](shinyApp.png)
***
https://bclark94.shinyapps.io/NextWordPredictorWebApp/

- Sleek, simple interface
- Fast predictions
- Low memory profile


Challenge - Create a Text Predictor
========================================================
- Create a natural language processing alorithm to predict next word given a text input

- Provided with large corpora of blogs, news, and twitter data

- How can you build a model that is accurate, quickly repsonds to the user, and has low memory requirements?

Algorithm
========================================================

- The core prediction engine used nGrams (1-4) and Markov chain with an optimized weighting found through cross validation.

- Over 200,000 words from various text sources considered in build of model

- Mathematical representation of weighted nGram model

$$ \begin{align*}
P(w_n|w_{n-1},w_{n-2},w_{n-3}) = & \lambda_0 P(w_n) + \\
& \lambda_1 P(w_n|w_{n-1}) + \\
& \lambda_2 P(w_n|w_{n-1},w_{n-2})+ \\
& \lambda_3 P(w_n|w_{n-1},w_{n-2},w_{n-3})
\end{align*} $$


- Achieves 21.34% accuracy while maintaining suststainable memory profile

Web App Implementation
========================================================

The front end applicating was developed in R shiny.

The app has been successfully deployed using shinyapps.io

Testing has been completed showing efficient memory usage and quick results

## https://bclark94.shinyapps.io/NextWordPredictorWebApp/

