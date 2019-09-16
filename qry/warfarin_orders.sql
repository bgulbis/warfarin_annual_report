WITH WARFARIN_PTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		CASE ORDERS.TEMPLATE_ORDER_ID
			WHEN 0 THEN CLINICAL_EVENT.ORDER_ID
			ELSE ORDERS.TEMPLATE_ORDER_ID
		END AS ORDER_ID
	FROM
		CLINICAL_EVENT,
		ENCOUNTER,
		ORDERS
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37558355 -- warfarin
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN 
			pi_to_gmt(DATE '2016-07-01', pi_time_zone(2, @Variable('BOUSER'))) 
			AND pi_to_gmt(DATE '2019-07-01', pi_time_zone(2, @Variable('BOUSER')))
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND (
			CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
			AND ENCOUNTER.ACTIVE_IND = 1
			AND ENCOUNTER.LOC_FACILITY_CD IN (
				3310, -- HH HERMANN
				3796, -- HC Childrens
				3821, -- HH Clinics
				3822, -- HH Trans Care
				3823 -- HH Rehab
			)
		)
		AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
)

SELECT 
	WARFARIN_PTS.ENCNTR_ID AS ENCOUNTER_ID,
	ORDERS.ORDER_ID AS ORDER_ID,
	TO_CHAR(pi_from_gmt(ORDERS.ORIG_ORDER_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ORDER_DATETIME,
	pi_get_cv_display(ENCNTR_LOC_HIST.MED_SERVICE_CD) AS MED_SERVICE,	
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT,
	pi_get_cv_display(ORDER_ACTION.COMMUNICATION_TYPE_CD) AS COMM_TYPE,
	pi_get_cv_display(PRSNL_ACTION_PERSONNEL.POSITION_CD) AS ORDER_BY_ROLE,
	pi_get_cv_display(PRSNL_ORDER_PROVIDER.POSITION_CD) AS PROVIDER_ROLE	
FROM
	ENCNTR_LOC_HIST,
	ORDER_ACTION,
	ORDERS,
	PRSNL PRSNL_ACTION_PERSONNEL,
	PRSNL PRSNL_ORDER_PROVIDER,
	WARFARIN_PTS
WHERE
	WARFARIN_PTS.ORDER_ID = ORDERS.ORDER_ID
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
	AND (
		ORDERS.ORDER_ID = ORDER_ACTION.ORDER_ID
		AND ORDER_ACTION.ACTION_TYPE_CD = 1376 -- Order
		AND ORDER_ACTION.ACTION_PERSONNEL_ID = PRSNL_ACTION_PERSONNEL.PERSON_ID
		AND ORDER_ACTION.ORDER_PROVIDER_ID = PRSNL_ORDER_PROVIDER.PERSON_ID
	)