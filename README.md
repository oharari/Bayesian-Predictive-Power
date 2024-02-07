# **About this application**
This tool was created to allow investigators conducting clinical trials to make educated decisions about the way they proceed with their  trial in light of data collected. Such decisions may take place either at the design stage or as part of a planned interim analysis in an adaptive trial. The tool is devided into separate tabs, to meets the user's needs at different stages of their trial.

A detailed description of the different features of the software and how they can be applied in clinical research can be found in the [**user manual**](BIAS_Manual.pdf).

Schematically, the user should use the tool as follows: 
* For design and analysis of phase II trials based on *Bayesian predictive power* (BPP) - 
  1. Use the *BPP Trial Design* tab to choose a predictive power cutoff for your phase II trial.
  2. Use the *BPP Calculations* tab to calculate the predictive power for each arm in your trial, to determine whether or not to progress it to the follow-up trial.
  
</div>
<div style="margin-bottom:10px;">
</div>  
* For interim analysis in a Bayesian adaptive trial, use the *Posterior Superiority* tab to calculate the *probability of superiority* of each arm and apply decision rules (stop early for futility/superiority) or adapt treatment allocation probabilities as per your design.

The user should be made aware that while the *BPP Calculations* and *Posterior Superiority* tabs make use of observed data, calculations performed in the *BPP Trial Design* tab are based on assumptions made regarding the true, underlying parameter values.

</div>
<div style="margin-bottom:35px;">
</div>

### **BPP Trial Design**
The purpose of this tab is to help investigators design a trial that either involves an interim analysis or will be followed up by another, independent trial (the same distinction as in the previous tab). This could be a phase II clinical trial to be followed up (potentially seamlessly) by a phase III confirmatory trial. The tool lets the user choose a predictive power cutoff that each arm must exceed in order to qualify to the follow-up trial.

For any given cutoff, the software calculates the probability of each arm getting selected, as well as the expected predictive power at interim analysis. Also displayed is the BPP distribution at the end of the first part of the trial. The **Selectivity** criterion - the probability of the best subset being selected, based on the number of treatments the user wishes to proceed with - is evaluated and maximized, with recommendation made to the user. 

To gain insight on the way the different outputs of the software are calculated, read [**the manuscript**](BPP_Paper.pdf).

</div>
<div style="margin-bottom:35px;">
</div>


### **BPP Calculations**
In this part of the application the user can insert data summaries (means and standard deviations for continuous outcomes, numbers of patients and events for dichotomous outcomes) for both a control and a treatment arm, in order to assess the posterior predictive probability of rejecting the null hypothesis (i.e. the **Bayesian Predictive Power, BPP**) in favour of either superiority of non-inferiority of the treatment arm versus the control some number of patients into the future. We make a distinction between two types of predictive power --

**1. Cross-trial:** the trial is informed by a preceeding, independent trial, but the eventual analysis will only be based on the most recent data collected, and 

**2. Within-trial:** we are conducting an interim analysis as part of a larger trial, and all previous data will go towards the final analysis.

Other than the predictive power for prespecified numbers of future patients, the app also allows the user to calculate the number of patients (per arm) required to hit a BPP target.


The technical details behind the implementation of the different elements of the software can be found in [**the manuscript**](BPP_Paper.pdf).

</div>
<div style="margin-bottom:35px;">
</div>


### **Posterior Superiority**
This part of the software offers the user the chance to apply Bayesian decision rules and response adaptive randomization (RRR) that are based on interim analysis. The trial's goal is to find the best treatment among multiple treatments, and adaptations are made according to the **probability of superiority** of each arm, that is: the posterior probability that said arm is superior to all other study arms. Calculation is based on large Monte Carlo samples drawn from the posterior distributions of the population means of the different study arms. The investigator may set threshold on this number to decide whether the trial can be stopped early, as well as drop study arms that show little promise along the way. 

The allocation probability to arm $i$ (i.e. the proportion of future patients to be randomized to it), whose probability of superiority is $p^{\text{sup}}_i$, is then - 
$$q_{i} \propto \left(p^{\text{sup}}_i\right)^{\gamma},$$
where $0\leq\gamma\leq1$ is a parameter selected by the user to balance between no response adaptation ($\gamma = 0$) and "strong" adaptation ($\gamma = 1$). 

The tool summarizes the above in a table. Posterior densities and $95\%$ Bayesian credible intervals are also displayed graphically.

* **Normal outcomes:**
Here observation are assumed to follow a Normal distribution, and the different study arms are compared in terms of their means. Assigning the mean parameters flat (*noninformative*) prior distributions, their posterior distributions will follow Student's $t$ distribution. 

* **Dichotomous outcomes:**
When the outcome of interest is a dichotomous one, assigning the event rate parameter a uniform prior distribution will result in a Beta posterior distribution. Here superiority of any arm to all others is defined in terms of its event rate parameter.


</div>
<div style="margin-bottom:10px;">
</div>  
Note that this tab only allows performing interim efficacy testing, comparing the evaluated probability of superiority against prespecified cutoffs. To deterime said cutoffs, one must first simulate the trial design for a large number of times to confirm that its operating characteristics such as power and type I error rate meet the requirements.
