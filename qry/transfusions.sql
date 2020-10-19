WITH WARFARIN_PTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID
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
	CLINICAL_EVENT.ENCNTR_ID AS ENCOUNTER_ID,
	TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS TRANSFUSE_DATETIME,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS EVENT,
	pi_get_cv_display(CE_PRODUCT.PRODUCT_CD) AS PRODUCT,
	CE_PRODUCT.PRODUCT_VOLUME AS VOLUME,
	pi_get_cv_display(CE_PRODUCT.PRODUCT_VOLUME_UNIT_CD) AS VOL_UNITS
FROM
	CE_PRODUCT,
	CLINICAL_EVENT,
	WARFARIN_PTS
WHERE
	WARFARIN_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CD = 33981 -- TRANSFUSED
	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN 
		pi_to_gmt(DATE '2016-07-01', pi_time_zone(2, @Variable('BOUSER'))) 
		AND pi_to_gmt(DATE '2019-07-05', pi_time_zone(2, @Variable('BOUSER')))
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.EVENT_ID = CE_PRODUCT.EVENT_ID(+)
