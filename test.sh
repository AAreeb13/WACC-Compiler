#!/bin/bash

# output results during interrupt
trap 'output_results' SIGINT

./init.sh

# update wacc examples for any changes
git submodule update --init --recursive

validPassed=0
syntaxErrPassed=0
semanticErrPassed=0

output_results() {
    validTotal=$(find "./wacc_examples/valid" -type f | wc -l | xargs)
    syntaxErrTotal=$(find "./wacc_examples/invalid/syntaxErr" -type f | wc -l | xargs)
    semanticErrTotal=$(find "./wacc_examples/invalid/semanticErr" -type f | wc -l | xargs)

    totalPassed=$((validPassed + syntaxErrPassed + semanticErrPassed))
    totalTotal=$((validTotal + syntaxErrTotal + semanticErrTotal))

    echo -e "\n=======INTEGRATION TEST RESULTS======="
    echo "Valid Tests: $validPassed/$validTotal"
    echo "Syntax Error Tests: $syntaxErrPassed/$syntaxErrTotal"
    echo "Semantic Error Tests: $semanticErrPassed/$semanticErrTotal"
    echo "Total: $totalPassed/$totalTotal"
    echo "======================================"
    exit 1
}


while read -r file; do
    echo "======= Running: $file ======="
    file_contents=$(cat "$file")
    ./compile "$file_contents"

    exit_code=$?
    if [ $exit_code == "0" ] && [[ $file == *"/valid/"* ]]; then
        validPassed=$((validPassed+1))
        echo "Result: Valid"
    elif [ $exit_code == "100" ] && [[ $file == *"/invalid/syntaxErr/"* ]]; then
        syntaxErrPassed=$((syntaxErrPassed+1))
        echo "Result: Syntax Error"
    elif [ $exit_code == "200" ] && [[ $file == *"/invalid/semanticErr/"* ]]; then
        semanticErrPassed=$((semanticErrPassed+1))
        echo "Result: Semantic Error"
    fi
    echo
done < <(find "./wacc_examples" -type f -name "*.wacc")

output_results