# Prepare the dataframe for Table 1
table1 = []

# Calculate the statistics for each disease
for disease in data['Disease'].unique():
    disease_data = data[data['Disease'] == disease]
    
    males = disease_data[disease_data['Sex'] == 'Male']
    females = disease_data[disease_data['Sex'] == 'Female']
    
    # Counts
    count_male = len(males)
    count_female = len(females)
    count_male_percentage = count_male / len(disease_data) * 100
    count_female_percentage = count_female / len(disease_data) * 100
    
    # Ages
    age_mean_male = males['Age'].mean()
    age_std_male = males['Age'].std()
    age_mean_female = females['Age'].mean()
    age_std_female = females['Age'].std()
    
    # Chi-square test for count
    if count_male > 0 and count_female > 0:
        _, count_pvalue, _, _ = chi2_contingency([[count_male, count_female]])
    else:
        count_pvalue = float('nan')
    
    # Independent t-test for age
    if count_male > 0 and count_female > 0:
        _, age_pvalue = ttest_ind(males['Age'], females['Age'])
    else:
        age_pvalue = float('nan')
    
    # Store the results in the dataframe
    table1.extend([
        {
            'Disease': disease,
            'Sex': 'Male',
            'n (%)': f'{count_male} ({count_male_percentage:.1f}%)',
            'Age mean (SD)': f'{age_mean_male:.2f} ({age_std_male:.2f})',
            'n p-value': f'{count_pvalue:.3f}',
            'Age p-value': f'{age_pvalue:.3f}'
        },
        {
            'Disease': disease,
            'Sex': 'Female',
            'n (%)': f'{count_female} ({count_female_percentage:.1f}%)',
            'Age mean (SD)': f'{age_mean_female:.2f} ({age_std_female:.2f})',
            'n p-value': f'{count_pvalue:.3f}',
            'Age p-value': f'{age_pvalue:.3f}'
        }
    ])

# Convert the list of dictionaries to a DataFrame
table1_df = pd.DataFrame(table1)

# Pivot the DataFrame to the desired format
table1_pivot = table1_df.pivot(index='Disease', columns='Sex')

# Convert the DataFrame to LaTeX code
latex_code = table1_pivot.to_latex(index=True, multirow=True)

# Replace NaN values with empty string in LaTeX code
latex_code = latex_code.replace('nan', '')

latex_code
