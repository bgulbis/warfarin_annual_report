SELECT DISTINCT
	ORDERS.ENCNTR_ID AS ENCOUNTER_ID,
	ORDERS.ORDER_ID AS ORDER_ID,
	TO_CHAR(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ORDER_DATETIME,
	pi_get_cv_display(ENCNTR_LOC_HIST.MED_SERVICE_CD) AS MED_SERVICE,	
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT	
FROM
	ENCNTR_LOC_HIST,
	ORDERS
WHERE
	ORDERS.ACTIVE_IND = 1
	AND ORDERS.CATALOG_CD IN (
		783444683, -- Pharmacy Dosing Service(Coumadin)
		680013891, -- Pharmacy Dosing Service(Warfarin)
		681345021 -- Pharmacy Dosing Service(Warfarin).
	)
	AND ORDERS.ORIG_ORD_AS_FLAG = 0
	AND	ORDERS.TEMPLATE_ORDER_FLAG IN (0, 1)
	AND ORDERS.TEMPLATE_ORDER_ID = 0
	AND ORDERS.ORIG_ORDER_DT_TM BETWEEN 
		pi_to_gmt(
			TO_DATE(
				@Prompt('Enter begin date', 'D', , mono, free, persistent, {'07/01/2017 00:00:00'}, User:80),
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			),
			'America/Chicago'
		)
		AND pi_to_gmt(
			TO_DATE(
				@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2020 00:00:00'}, User:81),
				pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
			),
			'America/Chicago'
		)
	AND (
		ORDERS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND ENCNTR_LOC_HIST.LOC_FACILITY_CD IN (
			3310, -- HH HERMANN
			3796, -- HC Childrens
			3821, -- HH Clinics
			3822, -- HH Trans Care
			3823 -- HH Rehab
		)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
		AND ORDERS.ORIG_ORDER_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				ELH.TRANSACTION_DT_TM <= ORDERS.ORIG_ORDER_DT_TM
				AND ORDERS.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
		)
	)
