#!/usr/local/redcraft/py/bin/python3
"""
    createRedcatFile.py
    by Julian Rachele Jun, 2021
    
    Creates .redcat file from .pdb and .schema file
    if schema is not provided, default schema is used.

    If --rdc flag is provided, create RDC files from .redcat file, with default order tensors of 
    3e-4 5e-4 -8e-4 for M1
    -4e-4 -6e-4 10e-4 for M2

"""

import glob
import argparse
import os
import re
import subprocess

from pathlib import Path

parser = argparse.ArgumentParser(description='Creates .redcat file from .pdb and optional .schema file')
parser.add_argument('pdbpath', type=str, help='Path to PDB file for input')
parser.add_argument('output', type=str, help='Path to desired .redcat output file')
parser.add_argument('--schema', type=str, help='Path to schema file, otherwise default is used')
parser.add_argument('--rdc', type=str, help='RDC prefix; if given it will produce 2 RDC files with the prefix')
parser.add_argument('--hz', type=int, help='Error in Hz to pass to REDCAT while generating simulated RDCs')



args = parser.parse_args()
pdbpath = args.pdbpath
output = args.output
schema = args.schema
rdc = args.rdc
hz = args.hz


DEFAULT_SCHEMA = """N C -1 6125 2
N H 0 24350 4
H C -1 -60400 3
CA HA 0 -60400 4
HA H 0 -240200 3
H HA -1 -240200 3"""

# load schema if exists, otherwise use default
if not schema:
    schema = DEFAULT_SCHEMA
else:
    with open(schema) as f:
        schema = schema.read()

# read through pdb file 
atom_coords = []
schema_dict = {}
for line in schema.split('\n'):
    [a1, a2, gap, maxrdc, error] = line.split()
    # Associate the schema relevant content with the two first atoms for later retrieval
    schema_dict[f"{a1} {a2}"] = (int(gap), int(maxrdc), int(error))

residues = []
# Populate atom data from PDB file
with open(pdbpath) as pdbfile:
    for line in pdbfile.readlines():
        if line.startswith("ATOM"):
            [_, _, atom, aa, _, res, x, y, z, _, _, _] = line.split()
            res = int(res)
            # if we need to append a new atom
            if (len(atom_coords) < res):
                atom_coords.append(dict())
                residues.append(aa)
            
            atom_coords[res-1][atom] = f"{x} {y} {z}"

def get_coords(res, atom):
    return (True, atom_coords[res][atom]) if atom in atom_coords[res] else (False, "999 999 999")

def output_line(a1, a2, res):
    (gap, maxRDC, error) = schema_dict[f"{a1} {a2}"]
    coords = ""
    (successA1, coordsA1) = get_coords(res, a1)
    # we only incorporate the gap on the second atom
    (successA2, coordsA2) = get_coords(res + gap, a2)
    if not (successA1 and successA2):
        coords = "999 999 999 999 999 999"
    else:
        coords = f"{coordsA1} {coordsA2}"

    return f"{coords} 999 {maxRDC} {error} /* {a1}-{a2} from {res + 1}*/\n"

# Write the REDCAT output file given the schema and PDB information
with open(output, 'w') as outputfile:
    for res, amino_acid in enumerate(residues):
        outputfile.write(output_line('N', 'C', res))
        outputfile.write(output_line('N', 'H', res))
        outputfile.write(output_line('H', 'C', res))
        outputfile.write(output_line('CA', 'HA', res))
        outputfile.write(output_line('HA', 'H', res))
        outputfile.write(output_line('H', 'HA', res))

# RDC format
RDC_FORMAT = """{res} 999 {res}
{i} {res} C {j} {res} N {r1} 1
{j} {res} N {j} {res} H {r2} 1
{i} {res} C {j} {res} H {r3} 1
{j} {res} CA {j} {res} HA {r4} 1
{j} {res} HA {j} {res} H {r5} 1
{i} {res} HA {j} {res} H {r6} 1
"""
# now that the .redcat file is created we can create RDCs 
if rdc is not None:
    with open(f"{rdc}.1", "w") as f:
        with open(f"{rdc}.2", "w") as f2:
            res = subprocess.run(['/usr/local/bin/redcat', 'redcatbin', '-i', f'{output}', '-c', '3e-4', '5e-4', '-8e-4', '0', '0', '0', 'n', f'{hz}'], stdout=subprocess.PIPE)
            res2 = subprocess.run(['/usr/local/bin/redcat', 'redcatbin', '-i', f'{output}', '-c', '-4e-4', '-6e-4', '10e-4', '0', '0', '0', 'n', f'{hz}'], stdout=subprocess.PIPE)
            # res = subprocess.run(['/usr/local/bin/redcat', 'redcatbin', '-i', f'{output}', '-c', '3e-4', '5e-4', '-8e-4', '0', '0', '0', f'{output}'], stdout=subprocess.PIPE)
            # res2 = subprocess.run(['/usr/local/bin/redcat', 'redcatbin', '-i', f'{output}', '-c', '-4e-4', '-6e-4', '10e-4', '0', '0', '0', f'{output}'], stdout=subprocess.PIPE)
            lines = res.stdout.decode('utf-8').split()
            lines = lines[10:-6] # trim the edges to just get the RDCs
            lines2 = res2.stdout.decode('utf-8').split()
            lines2 = lines2[10:-6] # trim the edges to just get the RDCs
            if (len(residues) != len(lines) // 6 or len(lines) % 6 != 0):
                print("Error: mismatched number of residues within RDCs")
                exit(-1)
            # split the RDCs into groups of 6
            RDCs = [lines[i:i+6] for i in range(0, len(lines), 6)]
            RDCs2 = [lines2[i:i+6] for i in range(0, len(lines2), 6)]
            for ind, (r1, r2, r3, r4, r5, r6) in enumerate(RDCs):
                formatted = RDC_FORMAT.format(
                    res=residues[ind],
                    i=ind,
                    j=ind+1,
                    r1=r1,
                    r2=r2,
                    r3=r3,
                    r4=r4,
                    r5=r5,
                    r6=r6
                )
                f.write(formatted)
            for ind, (r1, r2, r3, r4, r5, r6) in enumerate(RDCs2):
                formatted = RDC_FORMAT.format(
                    res=residues[ind],
                    i=ind,
                    j=ind+1,
                    r1=r1,
                    r2=r2,
                    r3=r3,
                    r4=r4,
                    r5=r5,
                    r6=r6
                )
                f2.write(formatted)
    

    

