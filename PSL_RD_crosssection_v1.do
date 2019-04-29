




global path_project "r:\\Project\\SeattleMinimumWage\\Stata\\Instability\\CH2\\"
global path_data "${path_project}data\\"
global path_output "${path_project}output\\sumstat\\"
global path_figures "${path_project}\\Stata\\Instability\\CH2\\figures\\"

use "${path_data}\PSL_cross_section_data_`sample_size'.dta"

drop if industry_drop ==1
drop if bad_data ==1
drop if multiest==1
keep if region ==1
drop if wagerate==.
drop if FTE_PSSTO <1

gen double R= FTE_PSSTO-4
gen temp_time = time +6
****RDDENSITY
*12 rows for 12 quarters (2011q1-2013q4)
mat T = J(12,5,.)

**RD density for all quarters pre and post to test for manipulation of the running variable
** computer generate bandwidth, unsure how to see the bins
forvalues i= 1/12 {
	rddensity R if temp_time ==`i'
	mat T[`i',1] = round(e(h_l),.001)
	mat T[`i',2] = round(e(h_r),.001)
	mat T[`i',3] = e(N_h_l)
	mat T[`i',4] = e(N_h_r)
	mat T[`i',5] = round(e(pv_q),.001)
	}
preserve
	drop _all
	svmat T
	outsheet using "${path_output}rddensity_`sample_size'.csv", comma replace
restore

**the next thing you'll want to do is play around with the bandwidth & the weighting estimator?
** computer 
mat H = J(12,5,.)
forvalues i= 1/12 {
	rddensity R if temp_time ==`i', h(1 1)
	mat T[`i',1] = round(e(h_l),.001)
	mat T[`i',2] = round(e(h_r),.001)
	mat T[`i',3] = e(N_h_l)
	mat T[`i',4] = e(N_h_r)
	mat T[`i',5] = round(e(pv_q),.001)
	}
preserve
	drop _all
	svmat H
	outsheet using "${path_output}rddensity_h22_`sample_size'.csv", comma replace
restore


mat T = J(12,5,.)
**then test out the kernel weighting
forvalues i= 1/12 {
	rddensity R if temp_time ==`i', kernel(uniform)
	mat T[`i',1] = round(e(h_l),.001)
	mat T[`i',2] = round(e(h_r),.001)
	mat T[`i',3] = e(N_h_l)
	mat T[`i',4] = e(N_h_r)
	mat T[`i',5] = round(e(pv_q),.001)
	}
preserve
	drop _all
	svmat T
	outsheet using "${path_output}rddensity_uniform_`sample_size'.csv", comma replace
restore
*then once you feel good about the yearquarter that is appropriate pre-period, this is the plot for the paper you'll use
rddensity R if yearquarter ==X, plot  genvars(temp) graph_options(title("RDDENSITY PLOT: ESD Data") xtitle("Firmsize-4"))
*A table 2 will have all the rddensity estimates for each yearquarter, and for both weightings?
*See if Caroline's paper shows how much of this to show-- it's also recommended to try these computer generated BW selects
*rddensity R, bwselect(diff) 
*rddensity R, fitselect(restricted)
 *but I'm not sure that they're necessary for the paper

*plot_range(1 7) plot_n(10 100) 

unab mainvars: wages_flow
*employed_flow hours_flow wages_flow wagerate spell_duration spell_hire spell_separation arc_wages_tot arc_hours_tot 

foreach outcome in `mainvars' {
	foreach q in 20111 20112 20113 20114 20121 20122 20123 20124 20131 20132 20133 20134 20141 20142 {		
		rdrobust hours_flow R if yearquarter == `q'  , p(1)
		estimate store `outcome'_`q'
	}
coefplot  `outcome'_20111 ||  `outcome'_20112 ||  `outcome'_20113 ||  `outcome'_20114 ||  `outcome'_20121 ||  `outcome'_20122 || `outcome'_20123 ||  ///
	`outcome'_20124 || `outcome'_20131  || `outcome'_20132 || `outcome'_20133 || `outcome'_20134 ||  `outcome'_20141 ||  `outcome'_20142,  ///
		drop(_cons) xline(0)  bycoefs   horizontal
*byopts(yrescale ) 
}
unab mainvars:  employed_flow hours_flow wages_flow wagerate spell_duration spell_hire spell_separation arc_wages_tot arc_hours_tot 

mat F = J(10,9,.)

** rdrobust: MSE data-driven bandwidth - local linear regression
rdrobust employed_flow R, p(1) 
mat aux = e(b)
mat F[1,1] = e(p)
mat F[2,1] = round(e(h_l),.001)
mat F[3,1] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,1] = round(`lb',.001)
mat F[5,1] = round(`ub',.001)
mat F[6,1] = round(e(pv_rb),.001)
mat F[7,1] = e(N_h_l)
mat F[8,1] = e(N_h_r)

rdrobust hours_tot R, p(1)
mat aux = e(b)
mat F[1,2] = e(p)
mat F[2,2] = round(e(h_l),.001)
mat F[3,2] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,2] = round(`lb',.001)
mat F[5,2] = round(`ub',.001)
mat F[6,2] = round(e(pv_rb),.001)
mat F[7,2] = e(N_h_l)
mat F[8,2] = e(N_h_r)

rdrobust wages_tot R, p(1)
mat aux = e(b)
mat F[1,3] = e(p)
mat F[2,3] = round(e(h_l),.001)
mat F[3,3] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,3] = round(`lb',.001)
mat F[5,3] = round(`ub',.001)
mat F[6,3] = round(e(pv_rb),.001)
mat F[7,3] = e(N_h_l)
mat F[8,3] = e(N_h_r)


rdrobust wagerate_tot R, p(1)
mat aux = e(b)
mat F[1,4] = e(p)
mat F[2,4] = round(e(h_l),.001)
mat F[3,4] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,4] = round(`lb',.001)
mat F[5,4] = round(`ub',.001)
mat F[6,4] = round(e(pv_rb),.001)
mat F[7,4] = e(N_h_l)
mat F[8,4] = e(N_h_r)

rdrobust spell_duration R, p(1)
mat aux = e(b)
mat F[1,5] = e(p)
mat F[2,5] = round(e(h_l),.001)
mat F[3,5] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,5] = round(`lb',.001)
mat F[5,5] = round(`ub',.001)
mat F[6,5] = round(e(pv_rb),.001)
mat F[7,5] = e(N_h_l)
mat F[8,5] = e(N_h_r)

rdrobust spell_hire R, p(1)
mat aux = e(b)
mat F[1,6] = e(p)
mat F[2,6] = round(e(h_l),.001)
mat F[3,6] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,6] = round(`lb',.001)
mat F[5,6] = round(`ub',.001)
mat F[6,6] = round(e(pv_rb),.001)
mat F[7,6] = e(N_h_l)
mat F[8,6] = e(N_h_r)

rdrobust spell_separation R, p(1)
mat aux = e(b)
mat F[1,7] = e(p)
mat F[2,7] = round(e(h_l),.001)
mat F[3,7] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,7] = round(`lb',.001)
mat F[5,7] = round(`ub',.001)
mat F[6,7] = round(e(pv_rb),.001)
mat F[7,7] = e(N_h_l)
mat F[8,7] = e(N_h_r)

rdrobust arc_wages_tot R, p(1)
mat aux = e(b)
mat F[1,8] = e(p)
mat F[2,8] = round(e(h_l),.001)
mat F[3,8] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,8] = round(`lb',.001)
mat F[5,8] = round(`ub',.001)
mat F[6,8] = round(e(pv_rb),.001)
mat F[7,8] = e(N_h_l)
mat F[8,8] = e(N_h_r)

rdrobust arc_hours_tot R, p(1)
mat aux = e(b)
mat F[1,9] = e(p)
mat F[2,9] = round(e(h_l),.001)
mat F[3,9] = round(aux[1,1],.001)
local lb = e(ci_l_rb)
local ub = e(ci_r_rb)
mat F[4,9] = round(`lb',.001)
mat F[5,9] = round(`ub',.001)
mat F[6,9] = round(e(pv_rb),.001)
mat F[7,9] = e(N_h_l)
mat F[8,9] = e(N_h_r)



preserve
	drop _all
	svmat F
	outsheet using "${path_output}rdrobust_falscohort_`sample_size'.csv", comma replace
restore



