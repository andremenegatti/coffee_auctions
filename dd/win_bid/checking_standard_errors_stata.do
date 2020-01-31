clear all
cap log close
set more 1
set dp comma

* Base de Dados
use "C:/Users/Dell/Desktop/sp_cafe_dd.dta", clear

reg win_bid i.mes i.comprasnet treat1 treat2

reg win_bid i.mes i.unidade_compradora i.comprasnet treat1 treat2

reg win_bid i.mes i.unidade_compradora i.marca_vencedor_principais futuro_defl arab_rob_defl i.comprasnet treat1 treat2

reg win_bid i.mes i.unidade_compradora i.marca_vencedor_principais futuro_defl arab_rob_defl i.comprasnet treat1 treat2, robust

reg win_bid i.mes i.unidade_compradora i.marca_vencedor_principais futuro_defl arab_rob_defl i.comprasnet treat1 treat2, cluster(comprasnet)

reg win_bid i.mes i.unidade_compradora i.marca_vencedor_principais futuro_defl arab_rob_defl i.comprasnet treat1 treat2, cluster(municipio)

reg win_bid i.mes i.unidade_compradora i.marca_vencedor_principais futuro_defl arab_rob_defl i.comprasnet treat1 treat2, cluster(unidade_compradora)
