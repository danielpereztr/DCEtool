\name{DCEtool}
\alias{DCEtool}
\title{Efficient and Accessible Experimental Designs for Discrete Choice Experiments: DCEtool}
\description{This app is intended to be a complete package for creating, surveying, and analysing discrete choice experiments. Although all these functionalities are available, the app can also be used only to obtain the design of a discrete choice experiment.

Introduce the parameters of the DCE in the 'Parameters' tab. Suppose you want to enter the first attribute in Table 1 of this article \doi{10.1016/j.jval.2016.04.004}. To do this, enter 'Efficacy' under 'Name of the attribute', '3' under 'Number of levels' and 'L1,L2,L3' under 'Levels' name'. Once you click on 'Add,' you will see the attribute appear on the right table. Try including more attributes. Once you have more then two attributes, enter the number of alternatives per choice set, the number of choice sets per respondent, choose if you want a null choice, and click on 'Save inputs'.

Once the inputs are saved, you can go to 'Design matrix' and click on 'Print the design matrix'. This procedure could take some seconds/minutes, depending on the design size and the computer's hardware. The design that will be printed is a pilot design, a D-efficient design estimated with the -idefix- package \doi{10.18637/jss.v096.i03} (Traets et al. 2020) with priors coefficients equal to zero. If the DCE is to be implemented outside the app, the design can be downloaded by clicking on the 'Download' button. The output is using 'Dummy coding', and the attributes and levels codification can be interpreted as in Table 3 of this \href{https://www.researchgate.net/publication/344360005_A_step-by-step_guide_to_design_implement_and_analyze_a_discrete_choice_experiment}{paper}. To implement the DCE in the app, you will need to give a name to the alternatives, an intro text, and an end text. Then you can proceed by clicking on the 'Create the survey' button.

In the next tab, 'Survey', a respondent can respond to the DCE by clicking on 'OK'. Once all the choice sets are completed, the end text will appear, and a new survey can be responded by clicking on 'OK'. If the researcher wants to use a sequential design, see the original proposal by Bliemer and Rose (2010) \doi{10.1108/9781849507738-006}, she can click on 'Next sequential design', and the information of the recorded responses will be used for increasing the efficiency of the design. Once the new priors appear under the survey, a new respondent can complete the survey by clicking on 'OK'. If all priors are equal to zero, the model coefficients are not significant at 5 per cent yet (probably due to a small number of responses). Adding more responses should lead to significant coefficients. Every time a survey is finished, you can move to the 'Results' tab.

In the 'Results' tab, you can download the data set, which is already coded to estimate a conditional logit model, or you can estimate the conditional logit model directly by clicking on 'Estimate a clogit'.
}
\usage{
DCEtool()
}

\value{
After introducing the inputs in the 'Parameters' tab, the pilot design can be downloaded in the 'Design matrix' tab using the 'Download' button. 

If the survey functionality of the app is used, each time a new response is recorded, the user can estimate a conditional logit and download the coded results in the 'Results' tab.


}

\examples{
if (interactive()){
#Example data
exampledata <- data.frame(name=c("Effectiveness", "Doses", "Adverse events", 
"Price"), lev=c(3, 2, 3, 3), label=c("70,80,90","1 dose,2 doses","0.1,0.5,1",
"50,100,150"))
values$df <- rbind(values$df, exampledata)
data.0 <- rbind(data.0, exampledata)

#2 alternatives sets per respondent
values$a <- 2

#8 choice sets per respondent
values$s <- 8

#opt-out
nula <- FALSE

#formatting the data
values$niveles <- unlist(c(values$df[2]))
values$atnames <- unlist(c(values$df[1]))
values$levnames <- unlist(c(values$df[3]))

#pilot design
des <- creator(values$niveles, nula, values$a, values$s)
pilot <- des[[1]]$design
}
}