WITH WARFARIN_PTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
	FROM
		CLINICAL_EVENT,
		ENCOUNTER
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37558355 -- warfarin
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN 
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
)

SELECT DISTINCT
	WARFARIN_PTS.ENCNTR_ID AS ENCOUNTER_ID,
	pi_get_cv_display(ORDERS.CATALOG_CD) AS HOME_MED
FROM
	ORDERS,
	WARFARIN_PTS
WHERE
	ORDERS.ACTIVITY_TYPE_CD = 378 -- Pharmacy
	AND ORDERS.CATALOG_CD = 9902731 -- warfarin
	AND ORDERS.CATALOG_TYPE_CD = 1363 -- Pharmacy
	AND WARFARIN_PTS.ENCNTR_ID = ORDERS.ENCNTR_ID
	AND WARFARIN_PTS.PERSON_ID = ORDERS.PERSON_ID
	AND ORDERS.ORIG_ORD_AS_FLAG = 2 -- Recorded / Home Meds
