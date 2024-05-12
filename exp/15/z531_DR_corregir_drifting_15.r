# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#------------------------------------------------------------------------------

options(error = function() {
  traceback(20)
  options(error = NULL)
  
  t <- format(Sys.time(), "%Y%m%d %H%M%S")
  cat( t, "\n",
    file = "z-Rabort.txt",
    append = TRUE
  )

  cat( t, "\n",
    file = "z-Rabort-hist.txt",
    append = TRUE
  )

  stop("exiting after script error")
})
#------------------------------------------------------------------------------

# Parametros del script
PARAM <- read_yaml( "parametros.yml" )


OUTPUT <- list()

#------------------------------------------------------------------------------

GrabarOutput <- function() {
  write_yaml(OUTPUT, file = "output.yml") # grabo output
}
#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos meses que estan
  dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter)]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[
    cliente_antiguedad == 3,
    ctrx_quarter_normalizado := ctrx_quarter * 1.2
  ]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  dataset[,t_activo_corriente := mcuentas_saldo+
            mplazo_fijo_dolares+
            mplazo_fijo_pesos+
            minversion1_pesos+
            minversion1_dolares+
            minversion2]
  dataset[,t_pasivo_corriente := vm_mconsumospesos+
            mprestamos_personales+
            mprestamos_prendarios+
            mprestamos_hipotecarios+
            mcuenta_debitos_automaticos+
            mttarjeta_master_debitos_automaticos+
            mpagodeservicios+
            mpagomiscuentas+
            mcomisiones_mantenimiento+
            mcomisiones_otras]
  dataset[,i_liquidez := t_activo_corriente/t_pasivo_corriente]
  dataset[,p_saldo_cc := mcuentas_saldo/ccuenta_corriente]
  dataset[,p_saldo_ca := mcuentas_saldo/ccaja_ahorro]
  dataset[,p_saldo_ctas := mcuentas_saldo/(ccaja_ahorro + ccuenta_corriente)]
  dataset[,i_saldo_debito := mcuentas_saldo/ctarjeta_debito]
  dataset[,i_consumo_payroll := (vm_mconsumospesos + vm_mconsumosdolares)/mpayroll]
  dataset[, cliente_antiguedad_anios := ceiling(cliente_antiguedad / 12)]
  dataset[, b_fidelidad := cut(cliente_antiguedad_anios, breaks = c(0, 2, 6, Inf), labels = c(0, 1, 2), right = FALSE)]
  dataset[,i_fidelidad1 := cliente_antiguedad * cliente_edad]
  dataset[,i_payroll_chq := cpayroll_trx/mcheques_emitidos]
  dataset[,p_cons_trans_m := mtarjeta_master_consumo / ctarjeta_master_transacciones]
  dataset[,p_cons_trans_v := mtarjeta_visa_consumo / ctarjeta_visa_transacciones]
  dataset[,p_cons_trans_vm := (p_cons_trans_m+p_cons_trans_v)/2]
  dataset[,i_rent_prod := mrentabilidad / cproductos]
  dataset[,t_prestamos := mprestamos_personales+ mprestamos_prendarios +
            mprestamos_hipotecarios]
  dataset[,ct_prestamos := cprestamos_personales+ cprestamos_prendarios +
            cprestamos_hipotecarios]
  dataset[,p_prestamos := t_prestamos/ct_prestamos]
  dataset[,c_inversiones := cplazo_fijo + cinversion1 + cinversion2
  ]
  dataset[,t_inversiones := mplazo_fijo_pesos + mplazo_fijo_dolares + minversion1_pesos + 
minversion1_dolares + minversion2]
  dataset[,c_seguros := cseguro_vida + cseguro_auto + cseguro_vivienda + 
cseguro_accidentes_personales]
  dataset[,c_acred_haberes := cpayroll_trx + cpayroll2_trx
  ]
  dataset[,t_acred_haberes := mpayroll + mpayroll2]
  dataset[,c_ctransferencias := ctransferencias_recibidas + ctransferencias_emitidas]
  dataset[,t_mtransferencias := mtransferencias_recibidas + mtransferencias_emitidas]
  dataset[,p_transferencias_recibidas := mtransferencias_recibidas / ctransferencias_recibidas]
  dataset[,p_transferencias_emitidas := mtransferencias_emitidas / ctransferencias_emitidas]
  dataset[,i_transferencias := (mtransferencias_emitidas + mtransferencias_recibidas) / (ctransferencias_recibidas + ctransferencias_emitidas)]
  # Verificar si los denominadores son diferentes de cero
dataset$denominador <- dataset$ctransferencias_recibidas + dataset$ctransferencias_emitidas

# Calcular p_ponderado_transeferencias
dataset$p_ponderado_transeferencias <- ifelse(dataset$denominador != 0,
                                             (dataset$mtransferencias_recibidas * dataset$ctransferencias_recibidas +
                                                dataset$mtransferencias_emitidas * dataset$ctransferencias_emitidas) /
                                               dataset$denominador,
                                             0)

  # Eliminar la columna de auxiliar de denominador si ya no la necesitas
  dataset <- subset(dataset, select = -c(denominador))
  dataset[,c_cheques := ccheques_depositados + ccheques_emitidos]  
  dataset[,t_cheques := mcheques_depositados + mcheques_emitidos] 
  dataset[, ratio_c_cheques := ifelse(ccheques_emitidos != 0, ccheques_depositados / ccheques_emitidos, NA)]
  dataset[, ratio_mcheques := ifelse(mcheques_emitidos != 0, mcheques_depositados / mcheques_emitidos, NA)]
  dataset[, p_cheques_depositados := ifelse(ccheques_depositados != 0, mcheques_depositados / ccheques_depositados, NA)]
  dataset[, p_cheques_emitidos := ifelse(ccheques_emitidos != 0, mcheques_emitidos / ccheques_emitidos, NA)]
  dataset[,t_operaciones_sucursal := ccajas_consultas + ccajas_depositos + ccajas_extracciones + ccajas_otras]
  dataset[, edad_bin := cut(cliente_edad, breaks = c(0, 30, 60, Inf), labels = c(0, 1, 2), right = FALSE)]
  dataset[,t_rentabilidad_mensual := mrentabilidad + mcomisiones + mactivos_margen + mpasivos_margen ]
  dataset[,p_trentabilidad_mensual := t_rentabilidad_mensual / mrentabilidad_annual     ]  
  dataset[,p_rentabilidad_mensual := mrentabilidad / mrentabilidad_annual ]
  dataset[,i_comisiones := (mcomisiones - mean(mcomisiones,na.rm = TRUE)) / sd(mcomisiones,na.rm = TRUE)     ]  
  dataset[,i_activos := (mactivos_margen - mean(mactivos_margen,na.rm = TRUE)) / sd(mactivos_margen,na.rm = TRUE)     ]  
  dataset[,i_pasivos := (mpasivos_margen  - mean(mpasivos_margen,na.rm = TRUE )) / sd(mpasivos_margen, na.rm = TRUE )     ]  
  dataset[, ratio_movimiento_capital := ifelse((mpayroll + mpayroll2 ) != 0, (mtransferencias_emitidas -  ccajas_extracciones) / (mpayroll + mpayroll2 ) , NA)]
  dataset[ ,ratio_endeudamiento :=   ifelse( (mcuentas_saldo + 
                                              mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   )!=0, (Visa_madelantopesos + Visa_madelantodolares + Master_madelantopesos +
                                                                                                                                 Master_madelantodolares + mpagomiscuentas + mpagodeservicios + mactivos_margen + 
                                                                                                                                 cdescubierto_preacordado + mtarjeta_visa_consumo + mtarjeta_master_consumo) / (mcuentas_saldo + 
                                                                                                                                                                                                                  mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   ), NA)
       ]  
  dataset[ ,ratio_ahorro :=   ifelse( (mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   )!=0, (mcuentas_saldo + mplazo_fijo_dolares + mplazo_fijo_pesos + minversion1_pesos + minversion1_dolares + minversion2) / ( mtransferencias_recibidas + mpayroll + mpayroll2 + mcheques_depositados   ), NA) ]  
  dataset[,p_atm_other := matm_other / catm_trx_other]
  dataset[,p_atm := matm / catm_trx]
  dataset[,p_forex_buy := mforex_buy / cforex_buy]
  dataset[,p_forex_sell := mforex_sell / cforex_sell]
  dataset[,ratio_cforex_buysell := cforex_buy / cforex_sell]
  dataset[,ratio_mforex_buysell := mforex_buy / mforex_sell]
  dataset[,p_mextraccion_autoservicio := mextraccion_autoservicio / matm]
  dataset[,p_cextraccion_autoservicio := cextraccion_autoservicio / catm_trx]
  dataset[ ,d_prestamos :=   ifelse( (cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios) > 0 ,1, 0)]  
  dataset[ ,d_seguros :=   ifelse( (cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales) > 0 ,1, 0)]  
  dataset[ ,d_cajas_ahorro :=   ifelse( (ccaja_ahorro) > 0 ,1, 0)]  
  dataset[ ,dcuenta_corriente :=   ifelse( (ccuenta_corriente) > 0 ,1, 0)] 
  dataset[ ,d_debitos_automaticos :=   ifelse( (ccuenta_debitos_automaticos) > 0 ,1, 0)]  
  dataset[ ,d_pagodeservicios :=   ifelse( (cpagodeservicios) > 0 ,1, 0)]  
  dataset[ ,d_pagomiscuentas :=   ifelse( (cpagomiscuentas) > 0 ,1, 0)]  
  dataset[ ,d_forex :=   ifelse( (cforex) > 0 ,1, 0)]  
  dataset[ ,d_forex_buy :=   ifelse( (cforex_buy) > 0 ,1, 0)]  
  dataset[ ,d_forex_sell :=   ifelse( (cforex_sell) > 0 ,1, 0)]  
  dataset[ ,d_transferencias_emitidas :=   ifelse( (ctransferencias_emitidas) > 0 ,1, 0)]  
  dataset[ ,d_uso_atm :=   ifelse( (catm_trx+catm_trx_other) > 0 ,1, 0)]  
  dataset[ ,d_cheques_emitidos :=   ifelse( ccheques_emitidos > 0 ,1, 0)]  
  dataset[ ,d_cheques_depositados :=   ifelse( ccheques_depositados > 0 ,1, 0)]  
  dataset[ ,d_operaciones_en_sucursal :=   ifelse( (
  ccajas_transacciones +
  ccajas_consultas +
  ccajas_depositos +
  ccajas_extracciones +
  ccajas_otras

    ) > 0 ,1, 0)]  
  dataset[,t_montos := mrentabilidad+mrentabilidad_annual+mcomisiones+mactivos_margen+mpasivos_margen+mcuenta_corriente_adicional+mcuenta_corriente+mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares+mcuentas_saldo+mautoservicio+mtarjeta_visa_consumo+mtarjeta_master_consumo+mprestamos_personales+mprestamos_prendarios+mprestamos_hipotecarios+mplazo_fijo_dolares+mplazo_fijo_pesos+minversion1_pesos+minversion1_dolares+minversion2+mpayroll+mpayroll2+mcuenta_debitos_automaticos+mttarjeta_master_debitos_automaticos+mpagodeservicios+mpagomiscuentas+mcajeros_propios_descuentos+mtarjeta_visa_descuentos+mtarjeta_master_descuentos+mcomisiones_mantenimiento+mcomisiones_otras+mforex_buy+mforex_sell+mtransferencias_recibidas+mtransferencias_emitidas+mextraccion_autoservicio+mcheques_depositados+mcheques_emitidos+mcheques_depositados_rechazados+mcheques_emitidos_rechazados+matm+Master_mfinanciacion_limite+Master_msaldototal+Master_msaldopesos+Master_msaldodolares+Master_mconsumospesos+Master_mconsumosdolares+Master_mlimitecompra+Master_madelantopesos+Master_madelantodolares+Master_mpagado+Master_mpagospesos+Master_mpagosdolares+Master_mconsumototal+Master_mpagominimo]
  #dataset[,pond_montos := t_montos/sum(dataset$t_montos)]
  #dataset[,pond_rentabilidad := t_rentabilidad_mensual/sum(dataset$t_rentabilidad_mensual)]
  dataset[,d_rentabilidad_mensual_neg := ifelse( (t_rentabilidad_mensual) < 0 ,1, 0)]
  dataset[,d_i_liquidez_negativa := ifelse( (i_liquidez) < 0 ,1, 0)]
  dataset[,d_ca_negativa := ifelse( (mcaja_ahorro) > 0 ,1, 0)]
  dataset[,d_cc_negativa := ifelse( (mcuenta_corriente ) > 0 ,1, 0)]
  dataset[,indice_dummy := d_ca_negativa-d_cc_negativa-d_cajas_ahorro+dcuenta_corriente+d_debitos_automaticos+
          d_pagodeservicios+d_pagomiscuentas+d_forex+d_forex_buy+d_forex_sell+d_transferencias_emitidas+d_uso_atm+
          d_cheques_emitidos+d_prestamos+d_seguros+d_i_liquidez_negativa-d_rentabilidad_mensual_neg]
  dataset[, d_uso_tarjeta_credito := ifelse(ctarjeta_visa_transacciones + ctarjeta_master_transacciones > 0, 1, 0)]
  dataset[, d_uso_tarjeta_debito := ifelse(ctarjeta_debito_transacciones  > 0, 1, 0)]
  dataset[, ratio_tarjdebito_tarjcredito := ifelse(ctarjeta_visa_transacciones + ctarjeta_master_transacciones == 0, 
                                                 ifelse(ctarjeta_debito_transacciones == 0, 0, ctarjeta_debito_transacciones), 
                                                 round(ctarjeta_debito_transacciones / (ctarjeta_visa_transacciones + ctarjeta_master_transacciones), 2))]
  dataset[, ratio_visa_consumototal_saldototal := Visa_mconsumototal / Visa_msaldototal ]
  dataset[, ratio_master_consumototal_saldototal := Master_mconsumototal / Master_msaldototal ]
  dataset[, t_deuda_tarjetacredito := Visa_msaldototal + Master_msaldototal ]
  dataset[, d_inversion := ifelse(cplazo_fijo + cinversion1 + cinversion2 > 0, 1, 0)]
  dataset[, ratio_sucursal_vs_hogar := (ccallcenter_transacciones + chomebanking_transacciones + cmobile_app_trx) / (ccajas_transacciones + ccajas_consultas + ccajas_depositos + ccajas_transacciones + ccajas_otras) ]
  dataset[,t_transacciones := ctrx_quarter + Master_cconsumos + Visa_cconsumos]
  dataset[,p_monto_transacciones := t_montos/t_transacciones]
  dataset[,d_status_0 := as.integer(Master_status == 0 | Visa_status == 0)]
  dataset[,d_status_6 := as.integer(Master_status == 6 | Visa_status == 6)]
  dataset[,d_status_7 := as.integer(Master_status == 7 | Visa_status == 7)]
  dataset[,d_status_9 := as.integer(Master_status == 9 | Visa_status == 9)]
  dataset[,i_endeudamiento_payroll := t_pasivo_corriente/t_acred_haberes]
  dataset[,i_endeudamiento_patrimonio := t_pasivo_corriente/(minversion1_pesos + minversion1_dolares + minversion2)]
  dataset[, d_recibe_acreditaciones := ifelse(cpayroll_trx + cpayroll2_trx  > 0, 1, 0)]
  dataset[, d_es_rentable := ifelse( mrentabilidad  > 0, 1, 0)]
  dataset[, d_tiene_tarjetascredito := ifelse( (ctarjeta_visa + ctarjeta_master)  > 0, 1, 0)]
  dataset[, r_cliente_prefiere_otro_banco := (mextraccion_autoservicio + mtransferencias_emitidas) /  (mpayroll + mpayroll2)  ]
  dataset[, r_comisiones_vs_ingresos := (mcomisiones + mactivos_margen) /  (mpayroll + mpayroll2)  ]
  dataset[,d_status_ok := ifelse((Master_status + Visa_status) == 0, 1, 0)]
  dataset[,d_delinquency := ifelse((Visa_delinquency == 1 | Master_delinquency == 1), 1,0)]
  dataset[,ratio_pagomin_haberes := (Master_mpagominimo + Visa_mpagominimo) / (mpayroll + mpayroll2)]
  dataset[,d_ratio_pagomin_haberes_ok := ifelse(ratio_pagomin_haberes<10,1,0)]
  dataset[,d_perfil_tipico_baja_1 := ifelse(d_ratio_pagomin_haberes_ok==0 & ccajas_otras==1 & d_status_ok ==0,1,0)]
  dataset[,d_perfil_tipico_baja_2 := ifelse((mcuenta_corriente_adicional | mprestamos_prendarios | mprestamos_hipotecarios | mplazo_fijo_pesos | minversion1_pesos | minversion1_dolares | minversion2 | mpayroll2 | cpayroll2_trx | mpagodeservicios | mcajeros_propios_descuentos | mtarjeta_visa_descuentos | mtarjeta_master_descuentos | mcomisiones_mantenimiento | mforex_buy | mforex_sell | Master_msaldodolares | Master_mconsumosdolares | Master_madelantopesos | Master_cadelantosefectivo | Visa_mconsumosdolares | Visa_madelantopesos | Visa_cadelantosefectivo),0,1)]
  dataset[,d_visa_finiciomora := ifelse(Visa_Finiciomora> 30,1,0)]
  dataset[, c_descuentos := ctarjeta_visa_descuentos + ctarjeta_master_descuentos]
  dataset[, t_descuentos := mtarjeta_visa_descuentos + mtarjeta_master_descuentos]
  dataset[, t_descuentos := (mtarjeta_visa_descuentos + mtarjeta_master_descuentos) / 
          (ctarjeta_visa_descuentos + ctarjeta_master_descuentos)]
  dataset[, t_movimientos_voluntarios := ctrx_quarter + ctarjeta_visa_transacciones + ctarjeta_master_transacciones]
  dataset[, i_transacciones := (t_transacciones - mean(t_transacciones,na.rm = TRUE )) / sd(t_transacciones,na.rm = TRUE) ]
  dataset[, i_t_movimientos_voluntarios := (t_movimientos_voluntarios - mean(t_movimientos_voluntarios,na.rm = TRUE )) / sd(t_movimientos_voluntarios,na.rm = TRUE) ]
  dataset[,i_rentabilidad_prestamos := mrentabilidad_annual / t_prestamos]
  dataset[,i_prestamoper_payroll :=  mprestamos_personales / t_acred_haberes]
  dataset[,t_prestamos_payroll :=  t_prestamos / t_acred_haberes]
  dataset[,i_payroll_t_transacciones := t_acred_haberes / t_transacciones]
  dataset[,i_callcenter_ctrx := ccallcenter_transacciones/ctrx_quarter]
  dataset[,t_cheques_neto := mcheques_depositados - mcheques_emitidos]
  dataset[,i_c_cheques_ctrx := c_cheques / ctrx_quarter]
  dataset[,i_c_cheques_em_ctrx := ccheques_emitidos/ ctrx_quarter]
  dataset[,i_c_cheques_dep_ctrx := ccheques_depositados / ctrx_quarter]
  dataset[,t_payroll_rentabildiad := t_acred_haberes - t_rentabilidad_mensual]
  dataset[,t_cash_flow := mcheques_depositados - mcheques_emitidos + t_acred_haberes - Visa_mpagado - Master_mpagado - mpagodeservicios -mpagomiscuentas -mcomisiones_mantenimiento -mcomisiones_otras-mtransferencias_emitidas+mtransferencias_recibidas - mcuenta_debitos_automaticos]
  dataset[,c_cheques_ok := ccheques_emitidos - ccheques_emitidos_rechazados]
  dataset[,i_saldo_antiguedad := mcuentas_saldo/cliente_antiguedad]
  dataset[,i_otras_comisiones := ccomisiones_otras / ccaja_ahorro]
  dataset[,i_comisiones := mcomisiones/mrentabilidad_annual]
  dataset[,t_pn := t_pasivo_corriente - t_activo_corriente]
  dataset[,c_transf_netas := ctransferencias_recibidas - ctransferencias_emitidas]
  dataset[,t_transf_netas := mtransferencias_recibidas - mtransferencias_emitidas]
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }
  return(dataset)
}
#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )

  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )

  tb_IPC <- data.table(
    paste0(PARAM$dataset_metadata$periodo) := vfoto_mes,
    "IPC" = vIPC
  )

  dataset[tb_IPC,
    on = c(PARAM$dataset_metadata$periodo),
    (campos_monetarios) := .SD * i.IPC,
    .SDcols = campos_monetarios
  ]
}

#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
      (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = eval(PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
      frank(get(campo), ties.method = "random") / .N, by = eval(PARAM$dataset_metadata$periodo)]

    dataset[get(campo) < 0, paste0(campo, "_rank") :=
      -frank(-get(campo), ties.method = "random") / .N, by = eval(PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
      (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
      by = eval(PARAM$dataset_metadata$periodo)]

    dataset[, (campo) := NULL]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
OUTPUT$PARAM <- PARAM
OUTPUT$time$start <- format(Sys.time(), "%Y%m%d %H%M%S")

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
PARAM$dataset <- paste0( "./", PARAM$input, "/dataset.csv.gz" )
PARAM$dataset_metadata <- read_yaml( paste0( "./", PARAM$input, "/dataset_metadata.yml" ) )

dataset <- fread(PARAM$dataset)


GrabarOutput()
write_yaml(PARAM, file = "parametros.yml") # escribo parametros utilizados

# primero agrego las variables manuales
if (PARAM$variables_intrames){
  dataset <- AgregarVariables_IntraMes(dataset)
} 

# ordeno dataset
setorderv(dataset, PARAM$dataset_metadata$primarykey)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
  "^(m|Visa_m|Master_m|vm_m|p_|t_)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(PARAM$metodo,
  "ninguno"        = cat("No hay correccion del data drifting"),
  "rank_simple"    = drift_rank_simple(campos_monetarios),
  "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
  "deflacion"      = drift_deflacion(campos_monetarios),
  "estandarizar"   = drift_estandarizar(campos_monetarios)
)


#------------------------------------------------------------------------------
# grabo el dataset

fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)

# copia la metadata sin modificar
write_yaml( PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
OUTPUT$dataset$ncol <- ncol(dataset)
OUTPUT$dataset$nrow <- nrow(dataset)
OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

# dejo la marca final
cat(format(Sys.time(), "%Y%m%d %H%M%S"), "\n",
  file = "z-Rend.txt",
  append = TRUE
)
