#!/bin/bash

# Test runner state
SHOW_ERRORS=false
LAST_CATEGORY=""
LAST_TYPE=""

print_menu() {
    echo
    echo "=== Interactive Test Runner ==="
    echo "0) Run single test file"
    echo "1) Run all tests"
    echo "2) Run parser/valid tests"
    echo "3) Run parser/invalid tests"
    echo "4) Run parser/recovery tests"
    echo "5) Run checker tests"
    echo "6) Toggle error messages (currently: $SHOW_ERRORS)"
    echo "7) Show errors for last run"
    echo "8) Rebuild compiler"
    echo "q) Quit"
    echo
}

run_parser_tests() {
    local category=$1
    local expect_success=$2
    local show_errors=${3:-$SHOW_ERRORS}

    LAST_CATEGORY="parser"
    LAST_TYPE="$category"

    echo "Testing parser/$category..."

    local passed=0
    local failed=0

    for test_file in tests/parser/$category/*.peb; do
        [ ! -f "$test_file" ] && continue

        name=$(basename "$test_file")
        printf "  %-25s " "$name"

        if [ "$show_errors" = "true" ]; then
            echo
            echo "    --- Error output ---"
            output=$(./pebc --parse "$test_file" 2>&1)
            result=$?
        else
            output=$(./pebc --parse "$test_file" 2>&1)
            result=$?
        fi

        if [ $result -eq 0 ]; then
            if [ "$expect_success" = "true" ]; then
                echo "PASS"
                ((passed++))
            else
                echo "FAIL (expected error)"
                ((failed++))
            fi
        else
            if [ "$expect_success" = "false" ]; then
                echo "PASS"
                ((passed++))
            else
                echo "FAIL (unexpected error)"
                ((failed++))
            fi
        fi

        if [ "$show_errors" = "true" ] && [ $result -ne 0 ]; then
            echo "$output" | sed 's/^/    /'
            echo
        fi
    done

    echo "  Results: $passed passed, $failed failed"
    return $failed
}

suggest_similar_tests() {
    local current_file=$1
    local current_dir=$(dirname "$current_file")

    echo "--- Similar Tests ---"
    echo "Tests in same directory:"
    ls "$current_dir"/*.peb 2>/dev/null | head -5

    echo
    echo "Tests with similar names:"
    find tests -name "*.peb" | grep -i "$(basename "$current_file" .peb)" | head -3
}

add_to_test_suite() {
    local test_file=$1
    echo "--- Add to Test Suite ---"
    echo "Current file: $test_file"
    echo
    echo "1) Copy to parser/valid"
    echo "2) Copy to parser/invalid"
    echo "3) Copy to parser/recovery"
    echo "4) Copy to checker/valid"
    echo "5) Copy to checker/invalid"
    echo

    read -p "Choose destination: " dest

    case $dest in
        1) cp "$test_file" tests/parser/valid/ && echo "Copied to parser/valid" ;;
        2) cp "$test_file" tests/parser/invalid/ && echo "Copied to parser/invalid" ;;
        3) cp "$test_file" tests/parser/recovery/ && echo "Copied to parser/recovery" ;;
        4) cp "$test_file" tests/checker/valid/ && echo "Copied to checker/valid" ;;
        5) cp "$test_file" tests/checker/invalid/ && echo "Copied to checker/invalid" ;;
        *) echo "Invalid choice" ;;
    esac
}

run_single_test() {
    echo "Available test files:"
    find tests -name "*.peb" | sort | nl
    echo
    read -p "Enter test number or path: " choice

    if [[ "$choice" =~ ^[0-9]+$ ]]; then
        test_file=$(find tests -name "*.peb" | sort | sed -n "${choice}p")
    else
        test_file="$choice"
    fi

    if [ ! -f "$test_file" ]; then
        echo "Test file not found: $test_file"
        return
    fi

    # Interactive test mode for single file
    while true; do
        echo
        echo "=== $(basename "$test_file") ==="
        echo "File: $test_file"
        echo
        echo "1) Parse only"
        echo "2) Parse + Check"
        echo "3) Full compile"
        echo "4) Show file content"
        echo "5) Edit file (opens \$EDITOR)"
        echo "6) Run with verbose output"
        echo "7) Compare with similar tests"
        echo "8) Add to test suite"
        echo "9) Rebuild compiler"         # <-- Add this line
        echo "b) Back to file selection"
        echo "m) Main menu"
        echo

        read -p "Choose action: " action

        case $action in
            1)
                echo "--- Parse Only ---"
                ./pebc --parse "$test_file"
                echo "Exit code: $?"
                ;;
            2)
                echo "--- Parse + Check ---"
                ./pebc --check-only "$test_file"
                echo "Exit code: $?"
                ;;
            3)
                echo "--- Full Compile ---"
                ./pebc "$test_file" -o test_output
                echo "Exit code: $?"
                ;;
            4)
                echo "--- File Content ---"
                cat -n "$test_file"
                ;;
            5)
                ${EDITOR:-nano} "$test_file"
                ;;
            6)
                echo "--- Verbose Output ---"
                ./pebc --parse "$test_file" --verbose 2>&1
                ;;
            7)
                suggest_similar_tests "$test_file"
                ;;
            8)
                add_to_test_suite "$test_file"
                ;;
            9)                            # <-- Add this case
                echo "--- Rebuilding Compiler ---"
                make pebc
                echo "Rebuild completed (exit code: $?)"
                ;;
            b|B)
                run_single_test  # Restart file selection
                return
                ;;
            m|M)
                return  # Back to main menu
                ;;
            *)
                echo "Invalid choice"
                ;;
        esac

        echo
        read -p "Press Enter to continue..."
    done
}

run_checker_tests() {
    local category=$1
    local expect_success=$2
    local show_errors=${3:-$SHOW_ERRORS}

    LAST_CATEGORY="checker"
    LAST_TYPE="$category"

    echo "Testing checker/$category..."

    local passed=0
    local failed=0

    for test_file in tests/checker/$category/*.peb; do
        [ ! -f "$test_file" ] && continue

        name=$(basename "$test_file")
        printf "  %-25s " "$name"

        if [ "$show_errors" = "true" ]; then
            echo
            echo "    --- Error output ---"
            output=$(./pebc --check-only "$test_file" 2>&1)
            result=$?
        else
            output=$(./pebc --check-only "$test_file" 2>&1)
            result=$?
        fi

        if [ $result -eq 0 ]; then
            if [ "$expect_success" = "true" ]; then
                echo "PASS"
                ((passed++))
            else
                echo "FAIL (expected error)"
                ((failed++))
            fi
        else
            if [ "$expect_success" = "false" ]; then
                echo "PASS"
                ((passed++))
            else
                echo "FAIL (unexpected error)"
                ((failed++))
            fi
        fi

        if [ "$show_errors" = "true" ] && [ $result -ne 0 ]; then
            echo "$output" | sed 's/^/    /'
            echo
        fi
    done

    echo "  Results: $passed passed, $failed failed"
    return $failed
}

show_last_errors() {
    if [ -z "$LAST_CATEGORY" ] || [ -z "$LAST_TYPE" ]; then
        echo "No previous test run to show errors for"
        return
    fi

    echo "Showing errors for $LAST_CATEGORY/$LAST_TYPE tests..."

    if [ "$LAST_CATEGORY" = "parser" ]; then
        run_parser_tests "$LAST_TYPE" "false" "true"
    elif [ "$LAST_CATEGORY" = "checker" ]; then
        run_checker_tests "$LAST_TYPE" "false" "true"
    fi
}

# Main interactive loop
while true; do
    print_menu
    read -p "Choose an option: " choice

    case $choice in
        0)
            run_single_test
            ;;
        1)
            echo "Building compiler..."
            make pebc
            run_parser_tests "valid" "true"
            run_parser_tests "invalid" "false"
            run_parser_tests "recovery" "false"
            run_checker_tests "valid" "true"
            run_checker_tests "invalid" "false"
            ;;
        2)
            run_parser_tests "valid" "true"
            ;;
        3)
            run_parser_tests "invalid" "false"
            ;;
        4)
            run_parser_tests "recovery" "false"
            ;;
        5)
            run_checker_tests "valid" "true"
            run_checker_tests "invalid" "false"
            ;;
        6)
            if [ "$SHOW_ERRORS" = "true" ]; then
                SHOW_ERRORS=false
            else
                SHOW_ERRORS=true
            fi
            echo "Error display set to: $SHOW_ERRORS"
            ;;
        7)
            show_last_errors
            ;;
        8)
            echo "Rebuilding compiler..."
            make pebc
            ;;
        q|Q)
            echo "Goodbye!"
            exit 0
            ;;
        *)
            echo "Invalid choice"
            ;;
    esac

    read -p "Press Enter to continue..."
done
