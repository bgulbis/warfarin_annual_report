#%%
import pandas as pd

#%%
raw_warf = pd.read_csv(
    'data/tidy/fy2019/warfarin_doses.csv', 
    usecols=['ENCOUNTER_ID', 'MED_DATETIME']
)
raw_warf['MED_DATE'] = pd.to_datetime(raw_warf['MED_DATETIME']) \
    .dt.floor(freq='D')
raw_warf.drop('MED_DATETIME', axis=1, inplace=True)
raw_warf.drop_duplicates(inplace=True)
raw_warf = raw_warf.groupby('ENCOUNTER_ID')
df_warf = raw_warf.agg(
    WARF_START=('MED_DATE', 'min'),
    WARF_STOP=('MED_DATE', 'max')
)

#%%
df_hgb = pd.read_csv(
    'data/tidy/fy2019/labs.csv', 
    index_col=['ENCOUNTER_ID', 'LAB_DATETIME'], 
    parse_dates=True
)
df_hgb = df_hgb.loc[df_hgb['LAB'] == 'Hgb']
df_hgb = df_hgb.sort_index()
df_hgb['RESULT_VALUE'] = pd.to_numeric(
    df_hgb['RESULT_VALUE'], errors='coerce'
)
df_hgb['LAB_DATE'] = pd.to_datetime(
    df_hgb.index.get_level_values('LAB_DATETIME')
)
df_hgb['LAB_DATE'] = df_hgb['LAB_DATE'].dt.floor(freq='D')

#%%
df_jn = df_hgb.join(df_warf)
df_jn.dropna(inplace=True)
df_jn['WARF_DAY'] = df_jn['LAB_DATE'] - df_jn['WARF_START']

df_jn = df_jn.xs(
    slice('2016-07-01', '2019-06-30'), 
    level='LAB_DATETIME', 
    drop_level=False
)

df_jn = df_jn.loc[df_jn['WARF_START'] <= \
    df_jn.index.get_level_values('LAB_DATETIME')]

df_jn = df_jn.loc[df_jn['WARF_STOP'] + pd.Timedelta(days=1) >= \
    df_jn.index.get_level_values('LAB_DATETIME')]

df_jn = df_jn.loc[df_jn['WARF_DAY'] < pd.to_timedelta(10, unit='days')]

df_jn['HGB_MAX'] = df_jn.reset_index(level='ENCOUNTER_ID') \
    .groupby('ENCOUNTER_ID').RESULT_VALUE.rolling('48h').max()
df_jn['HGB_CHG'] = df_jn['RESULT_VALUE'] - df_jn['HGB_MAX']

#%%
df_drop = df_jn.loc[df_jn['HGB_CHG'] <= -2].copy()
df_drop = df_drop.reset_index().groupby('ENCOUNTER_ID').min()

#%%
df_drop.to_csv('data/final/hgb_drop_fy19.csv')
