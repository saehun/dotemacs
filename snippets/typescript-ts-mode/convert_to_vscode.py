#!/usr/bin/env python3
import json
import os
import re

snippets = {}

def process_snippet_file(file_path):
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Check if file has key field
        key_match = re.search(r'# key:\s*(.+?)(?:\n|$)', content)
        if not key_match:
            return None
        
        key = key_match.group(1).strip()
        
        # Extract name
        name_match = re.search(r'# name\s*:\s*(.+?)(?:\n|$)', content)
        name = name_match.group(1).strip() if name_match else os.path.basename(file_path)
        
        # Find where the template starts (after # --)
        template_match = re.search(r'# --\n(.*)', content, re.DOTALL)
        if not template_match:
            return None
        
        template = template_match.group(1).rstrip()
        
        # Split template into lines
        body = template.split('\n')
        
        # Handle yasnippet expressions that can't be directly translated
        for i, line in enumerate(body):
            # Handle complex Emacs Lisp expressions
            if '`(' in line:
                # First, try to match common patterns
                if "string-inflection-pascal-case-function" in line:
                    # For Pascal case function, extract the filename component and make a placeholder
                    body[i] = re.sub(r'\$\{1:`\([^`]*\)`\}', '${1:ComponentName}', line)
                elif "capitalize yas-text" in line:
                    # For capitalizing yas-text, transform to capitalize the placeholder
                    match = re.search(r'\$\{(\d+):.*?\$\{(\d+):\$\(capitalize yas-text\)\}', line)
                    if match:
                        num, cap_num = match.groups()
                        body[i] = re.sub(r'\$\{' + num + r':.*?\$\{' + cap_num + r':\$\(capitalize yas-text\)\}', 
                                        '${' + num + ':value}, set${' + num + ':Value}', line)
                    else:
                        body[i] = re.sub(r'\$\{(\d+):\$\(capitalize yas-text\)\}', '${\\1:Value}', line)
                else:
                    # Replace other complex expressions with a placeholder
                    body[i] = re.sub(r'`\([^`]*\)`', '${1:placeholder}', line)
        
        return {
            'prefix': key,
            'body': body,
            'description': name
        }
    except Exception as e:
        print(f"Error processing {file_path}: {str(e)}")
        return None

# Process files from the list
with open('files_with_keys.txt', 'r') as file_list:
    for line in file_list:
        file_path = line.strip()
        if not file_path or not os.path.exists(file_path):
            continue
        
        snippet = process_snippet_file(file_path)
        if snippet:
            # Use the file name as the snippet name
            snippet_name = os.path.basename(file_path)
            if '/' in file_path:
                # For nested files, include directory in name
                rel_path = file_path[2:]  # Remove './'
                snippet_name = rel_path.replace('/', '_')
            
            snippets[snippet_name] = snippet

# Write the snippets to a JSON file
with open('vscode-snippets.json', 'w') as f:
    json.dump(snippets, f, indent=2, sort_keys=True, ensure_ascii=False)
    
print(f"Converted {len(snippets)} snippets to VSCode format in vscode-snippets.json")