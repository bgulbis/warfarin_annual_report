WITH WARFARIN_PTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.DISCH_DT_TM
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
	ENCOUNTER.ENCNTR_ID AS READMIT_ID,
	TO_CHAR(pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS READMIT_DATETIME,	
	ENCOUNTER.REG_DT_TM - WARFARIN_PTS.DISCH_DT_TM AS READMIT_DAYS,
	pi_get_cv_display(LOC_FACILITY_CD) AS READMIT_FACILITY,
	pi_get_cv_display(ENCOUNTER.ENCNTR_TYPE_CD) AS READMIT_TYPE
FROM
	ENCOUNTER,
	WARFARIN_PTS
WHERE
	WARFARIN_PTS.PERSON_ID = ENCOUNTER.PERSON_ID
	AND ENCOUNTER.ACTIVE_IND = 1
	AND ENCOUNTER.REG_DT_TM BETWEEN WARFARIN_PTS.DISCH_DT_TM AND WARFARIN_PTS.DISCH_DT_TM + 30
	AND ENCOUNTER.ENCNTR_TYPE_CD IN (
		29527, -- Emergency
		29532, -- Inpatient
		29540 -- Observation
	)
