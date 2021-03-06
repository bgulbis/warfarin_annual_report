SELECT DISTINCT
	ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
    TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE,
	TO_CHAR(pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS ADMIT_DATETIME,
	TO_CHAR(pi_from_gmt(ENCOUNTER.DISCH_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS DISCH_DATETIME,
	pi_get_cv_display(PERSON.RACE_CD) AS RACE,
	pi_get_cv_display(PERSON.SEX_CD) AS SEX,
	pi_get_cv_display(ENCOUNTER.DISCH_DISPOSITION_CD) AS DISCH_DISPOSITION
FROM
	CLINICAL_EVENT,
	ENCOUNTER,
	PERSON
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
	AND (
		ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
		AND PERSON.ACTIVE_IND = 1
	)
