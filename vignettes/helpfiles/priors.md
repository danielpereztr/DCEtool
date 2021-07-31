Usually, the priors are obtained from pilot experiments. If no priors are available, or the user is creating a pilot experiment, the drop-down list should specify "Zero (pilot design)". However, if the user knows some prior values, the drop-down list can be changed to "Personalized priors".

When "Personalized priors" are selected, the textbox below needs to contain the prior parameters in order and separated by commas. To introduce the prior parameters, the user needs to remember that: 

- The number of parameters should be equal to l-k (+1 if there is an opt-out alternative) [where l is the total number of levels and k is the total number of parameters]
- If there is an op-out option, the prior coefficient of the null option should go in first place. 
- The rest of parameters must be written in the same order in which the attributes and levels were specified. 
- A comma must be written between each coefficient. 
- The coefficient of the first level of each attribute must be always omitted (in dummy coding, this coefficient is always constricted to zero)

The following example can be applied to the example attributes and levels with a null option: 

| null  | effect_80 | effect_90 | doses_2 | adv_1in500 | adv_1in100 | cost_150 | cost_200 |
| ----- | --------- | --------- | ------- | ---------- | ---------- | -------- | -------- |
| -0.5, | 0.5,      | 1.2,      | -0.1,   | -0.2,      | -0.7,      | -0.1,    | -0.6     |

