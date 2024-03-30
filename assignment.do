clear all

global struc_dir "/Users/ynbsztl/Documents/LaTeX/上课笔记/研二下/贸易与发展工作坊-2023Fall/3.31due"
cd "$struc_dir"
import delimited "$struc_dir/OTC_Data_forStata.csv"

*col 1
	bysort product: egen price_carton = mean(price)

*col 2
foreach j in price cost{
	gen `j'_50tab_equiv = . 
	local carton_25 1 4 7 
	foreach i in `carton_25'{
		replace `j'_50tab_equiv = `j'*2 if product == `i'
	}

	local carton_50 3 6 9 11
	foreach i in `carton_50'{
		replace `j'_50tab_equiv = `j'/2 if product == `i'
	}

	replace `j'_50tab_equiv = `j' if missing(`j'_50tab_equiv)
}	
	
foreach j in quantity{
	gen `j'_50tab_equiv = . 
	local carton_25 1 4 7 
	foreach i in `carton_25'{
		replace `j'_50tab_equiv = `j'/2 if product == `i'
	}

	local carton_50 3 6 9 11
	foreach i in `carton_50'{
		replace `j'_50tab_equiv = `j'*2 if product == `i'
	}

	replace `j'_50tab_equiv = `j' if missing(`j'_50tab_equiv)
}
	
*col 3
bysort product: egen cost_50tab_equiv_mean = mean(cost_50tab_equiv)


*col 4
bysort week store: egen xx = sum(quantity_50tab_equiv)
bysort week store product: egen market_share = mean(quantity_50tab_equiv)
replace market_share = market_share/xx
drop xx

bysort product: egen market_share_mean = mean(market_share)
tab market_share_mean

*col 5
gen tab =.
replace tab = 25 if inlist(product,1,4,7)
replace tab = 50 if inlist(product,2,5,8,10)
replace tab = 100 if inlist(product,3,6,9,11)
bysort product: egen sum_q = sum((quantity*tab)/50)
bysort product: egen sum_c = sum(count)
tab sum_q
tab sum_c

bysort product: gen xx = sum_q/sum_c

gen Share_of_potent_sum = sum(xx)
bysort product: gen Marketshare = xx/Share_of_potent_sum
bysort product: egen Share_of_potent = mean(xx)
drop xx
tab Share_of_potent

*col 6
bysort product: egen prop_promo = mean(promotion)
tab prop_promo

export delimited using "$struc_dir/OTC_Data_forStata_modified.csv", replace
exit

