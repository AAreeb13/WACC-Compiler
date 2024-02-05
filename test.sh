#!/bin/bash

# output results during interrupt
trap 'output_results; exit 2' SIGINT

./init.sh

# update wacc examples for any changes
git submodule update --init --recursive

# set to 0 for full check
syntaxCheckOnly=1

validPassed=0
syntaxErrPassed=0
semanticErrPassed=0

validComplete=0
syntaxErrComplete=0
semanticErrComplete=0

validCurrent=0
syntaxErrCurrent=0
semanticErrCurrent=0


output_results() {
    validTotal=$(find "./wacc_examples/valid" -type f | wc -l | xargs)
    syntaxErrTotal=$(find "./wacc_examples/invalid/syntaxErr" -type f | wc -l | xargs)
    semanticErrTotal=$(find "./wacc_examples/invalid/semanticErr" -type f | wc -l | xargs)

    totalPassed=$((validPassed + syntaxErrPassed + semanticErrPassed))
    totalTotal=$((validTotal + syntaxErrTotal + semanticErrTotal))

    echo -e "\n=======INTEGRATION TEST RESULTS======="
    echo "Valid Tests: $validPassed/$validCurrent (total: $validTotal)"
    echo "Syntax Error Tests: $syntaxErrPassed/$syntaxErrCurrent (total: $syntaxErrTotal)"
    echo "Semantic Error Tests: $semanticErrPassed/$semanticErrCurrent (total: $semanticErrTotal)"
    echo "Total: $totalPassed/$totalTotal"
    echo "======================================"

    if [ "$validPassed" -eq "$validTotal" ]; then
        validComplete=1
    fi

    if [ "$syntaxErrPassed" -eq "$syntaxErrTotal" ]; then
        syntaxErrComplete=1
    fi

    if [ "$semanticErrPassed" -eq "$semanticErrTotal" ]; then
        semanticErrComplete=1
    fi
}


while read -r file; do
    echo "======= Running: $file ======="
    file_contents=$(cat "$file")
    ./compile "$file_contents"

    exit_code=$?
    if [[ $file == *"/valid/"* ]]; then
        if [ $exit_code == "0" ]; then
            validPassed=$((validPassed+1))
            echo "Result: Valid"
        fi
        validCurrent=$((validCurrent+1))
    elif [[ $file == *"/invalid/syntaxErr/"* ]]; then
        if [ $exit_code == "100" ]; then
            syntaxErrPassed=$((syntaxErrPassed+1))
            echo "Result: Syntax Error"
        fi
        syntaxErrCurrent=$((syntaxErrCurrent+1))
    elif [[ $file == *"/invalid/semanticErr/"* ]]; then
        if [ $exit_code == "200" ]; then
            semanticErrPassed=$((semanticErrPassed+1))
            echo "Result: Semantic Error"
        fi
        semanticErrCurrent=$((semanticErrCurrent+1))
    fi
    echo
done < <(find "./wacc_examples" -type f -name "*.wacc")

output_results

if [ $syntaxCheckOnly -eq 1 ] ; then
    if [ $validComplete -eq 1 ] && [ $syntaxErrComplete -eq 1 ]; then
        exit 0
    else
        exit 1
    fi
else
    if [ $validComplete -eq 1 ] && [ $syntaxErrComplete -eq 1 ] && [ $semanticErrComplete -eq 1 ]; then
        exit 0
    else
        exit 1
    fi
fi