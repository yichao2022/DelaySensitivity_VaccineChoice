#!/usr/bin/env python3
"""Generate Version 1 design matrix table for Appendix B from Fielded_DCE_Design.csv.

Fixes origin reversal bug: appendix had Domestic/Imported swapped vs the
authoritative CSV for all 12 Version-1 profiles.
"""
import csv
import re

SE_MAP = {'1': 'Mild', '2': 'Moderate', '3': 'Severe'}

def wait_str(m):
    return {'0': '0 months', '1': '1 month', '2': '2 months', '3': '3 months', '6': '6 months'}[m]

def eff_str(v):
    return f'{float(v)*100:.0f}\\%'

def build_table(rows):
    lines = []
    lines.append('\\begin{table}[htbp]')
    lines.append('\\centering')
    lines.append('\\caption{Complete design matrix for Version 1 ($n=250$ respondents). Opt-out alternative available in every set.}')
    lines.append('\\label{tab:design_matrix}')
    lines.append('\\small')
    lines.append('\\begin{tabular}{clccccc}')
    lines.append('\\hline')
    lines.append('Set & Alt & Waiting time & Efficacy & Side-effect profile & Cash & Origin \\\\')
    lines.append('\\hline')
    for r in rows:
        lines.append(
            f"{r['Task']} & {r['Alt']} & {wait_str(r['WaitTime_months'])} & "
            f"{eff_str(r['Efficacy'])} & {SE_MAP[r['SideEffects']]} & "
            f"{r['Cash_RMB']} RMB & {r['Origin']} \\\\"
        )
    lines.append('\\hline')
    lines.append('\\end{tabular}')
    lines.append('\\end{table}')
    return '\n'.join(lines)

# Read authoritative CSV
rows = []
with open('Fielded_DCE_Design.csv') as f:
    for r in csv.DictReader(f):
        if r['Version'] == '1':
            rows.append(r)
assert len(rows) == 12, f'expected 12 Version-1 rows, got {len(rows)}'

new_table = build_table(rows)

# Replace table block in appendix_B_choice_card.tex
path = 'appendix_B_choice_card.tex'
tex = open(path).read()
pattern = re.compile(r'\\begin\{table\}\[htbp\].*?\\end\{table\}', re.DOTALL)
matches = pattern.findall(tex)
assert len(matches) >= 2, f'expected >=2 tables, found {len(matches)}'

# The second table is tab:design_matrix (Version 1 full matrix)
# Replace only the one containing \\label{tab:design_matrix}
target = [m for m in matches if 'tab:design_matrix' in m]
assert len(target) == 1, f'expected exactly 1 design_matrix table, found {len(target)}'
tex = tex.replace(target[0], new_table)

open(path, 'w').write(tex)
print(f'Replaced design_matrix table ({len(target[0])} -> {len(new_table)} chars)')