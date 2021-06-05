# DCEtool
Complete package for creating, surveying, and analysing Discrete Choice Experiments

## The app
This app is intended to be a complete package for creating, surveying, and analysing discrete choice experiments. Although all these functionalities are available, the app can also be used only to obtain the design of a discrete choice experiment.

## How does it work?
Introduce the parameters of the DCE in the 'Parameters' tab. Suppose you want to enter the first attribute in Table 1 of this [article](https://doi.org/10.1016/j.jval.2016.04.004). To do this, enter 'Efficacy' under 'Name of the attribute', '3' under 'Number of levels' and 'L1,L2,L3' under 'Levels' name'. Once you click on 'Add,' you will see the attribute appear on the right table. Try including more attributes. Once you have more then two attributes, enter the number of alternatives per choice set, the number of choice sets per respondent, choose if you want a null choice, and click on 'Save inputs'.

Once the inputs are saved, you can go to 'Design matrix' and click on 'Print the design matrix'. This procedure could take some seconds/minutes, depending on the design size and the computer's hardware. The design that will be printed is a pilot design, a D-efficient design estimated with the [-idefix-](http://dx.doi.org/10.18637/jss.v096.i03) package (Traets et al. 2020) with priors coefficients equal to zero. If the DCE is to be implemented outside the app, the design can be downloaded by clicking on the 'Download' button. The output is using 'Dummy coding', and the attributes and levels codification can be interpreted as in Table 3 of this [paper](https://www.researchgate.net/publication/344360005_A_step-by-step_guide_to_design_implement_and_analyze_a_discrete_choice_experiment). To implement the DCE in the app, you will need to give a name to the alternatives, an intro text, and an end text. Then you can proceed by clicking on the 'Create the survey' button.

In the next tab, 'Survey', a respondent can respond to the DCE by clicking on 'OK'. Once all the choice sets are completed, the end text will appear, and a new survey can be responded by clicking on 'OK'. If the researcher wants to use a sequential design, see the original proposal by [Bliemer and Rose (2010)](https://doi.org/10.1108/9781849507738-006), she can click on 'Next sequential design', and the information of the recorded responses will be used for increasing the efficiency of the design. Once the new priors appear under the survey, a new respondent can complete the survey by clicking on 'OK'. If all priors are equal to zero, the model coefficients are not significant at 5% yet (probably due to a small number of responses). Adding more responses should lead to significant coefficients. Every time a survey is finished, you can move to the 'Results' tab.

In the 'Results' tab, you can download the data set, which is already coded to estimate a conditional logit model, or you can estimate the conditional logit model directly by clicking on 'Estimate a clogit'.

## About
This app was built on top of the [-idefix-](http://dx.doi.org/10.18637/jss.v096.i03) package by Traets et al. (2020) to simplify the task of creating DCEs and feasibly implement [Bliemer and Rose (2010) sequential design approach](https://doi.org/10.1108/9781849507738-006).

The app was developed in R by Daniel Pérez Troncoso at the Department of Applied Economics at the University of Granada. Contact: danielperez@ugr.es

## License 
Copyleft. License GPL-3

## Cite
Please, cite as Daniel Pérez-Troncoso (2021) DCE tool. [https://github.com/danielpereztr/DCEtool/](https://github.com/danielpereztr/DCEtool/)
