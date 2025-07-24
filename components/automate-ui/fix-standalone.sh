#!/bin/bash

# Script to add standalone: false to all Angular components that don't have it

echo "Adding standalone: false to Angular components..."

# Find all component files and add standalone: false if not already present
find src/app -name "*.component.ts" -type f | while read -r file; do
    # Check if the file already has standalone property
    if ! grep -q "standalone:" "$file"; then
        # Check if file has @Component decorator
        if grep -q "@Component(" "$file"; then
            echo "Fixing $file"
            # Use sed to add standalone: false after the last property in @Component
            sed -i '' 's/@Component({/@Component({\
  standalone: false,/g' "$file"
        fi
    fi
done

# Also fix directive files
find src/app -name "*.directive.ts" -type f | while read -r file; do
    if ! grep -q "standalone:" "$file"; then
        if grep -q "@Directive(" "$file"; then
            echo "Fixing directive $file"
            sed -i '' 's/@Directive({/@Directive({\
  standalone: false,/g' "$file"
        fi
    fi
done

# Also fix pipe files
find src/app -name "*.pipe.ts" -type f | while read -r file; do
    if ! grep -q "standalone:" "$file"; then
        if grep -q "@Pipe(" "$file"; then
            echo "Fixing pipe $file"
            sed -i '' 's/@Pipe({/@Pipe({\
  standalone: false,/g' "$file"
        fi
    fi
done

echo "Done! All components, directives, and pipes should now have standalone: false"
