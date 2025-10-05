---
description: Create comprehensive Playwright E2E tests using multi-agent workflow
argument-hint: <feature-description> [--url <page-url>] [--test-dir <directory>]
category: testing
---

Create a comprehensive Playwright E2E test for: $ARGUMENTS

## Workflow Overview

This command orchestrates a multi-agent workflow to create production-ready Playwright tests:

1. **Workspace Setup** - Create isolated workspace per feature
2. **Test Directory Detection** - Find where tests should be created
3. **Requirements Analysis** - Clarify what needs to be tested
4. **Page Analysis** - FIRST: Understand the actual page completely
5. **Test Planning Loop** - Create plans that match page reality
   - Test Planner creates/revises plan based on page knowledge
   - Page Analyzer validates plan is mappable
   - Loop until all steps mappable (max 3 iterations)
6. **Test Writing** - Generate Playwright code directly in test directory
7. **Test Execution** - Run tests from proper location with existing setup
8. **Final Review** - Present results and offer iterations

## Workspace Structure

**Workflow artifacts** (documentation):
```
.claude/playwright-workflow/[feature-name]/
├── .config                   (Workspace configuration)
├── requirements.md          (Requirements analysis - single file)
├── page-analysis.md         (Initial discovery - never changes after discovery)
│
├── iterations/              (Version history - audit trail)
│   ├── test-plan-v1.md     (Plan iteration 1)
│   ├── validation-v1.md    (Validation iteration 1)
│   ├── test-plan-v2.md     (Plan iteration 2 - revised)
│   ├── validation-v2.md    (Validation iteration 2)
│   └── ...                 (More iterations if needed)
│
├── test-plan.md            (CURRENT plan - copy of latest version)
├── validation.md           (CURRENT validation - copy of latest)
├── draft-test.ts           (Reference copy of generated test)
└── test-execution.md       (Test results from subagent)
```

**Actual test file** (in your test directory):
```
tests/                      (or your custom test location)
└── [feature-name].spec.ts  (Working test file)
```

**Benefits:**
- ✅ Complete audit trail in `iterations/` folder
- ✅ Easy access to current versions (test-plan.md, validation.md)
- ✅ Can review evolution: see what changed between iterations
- ✅ Great for debugging: "validation-v1 failed because..."
- ✅ Learning opportunity: understand refinement process

**After workflow completes:**
- Test file is ready to run: `npx playwright test tests/[feature-name].spec.ts`
- Full iteration history preserved for review
- Workspace can be archived with complete documentation

## Instructions

### Step 0: Initialize Workspace and Detect Test Directory

Before starting the workflow, set up the workspace and locate the test directory.

**Your tasks:**

1. **Parse arguments:**
   - Extract feature description from $ARGUMENTS
   - Check for --url flag (optional)
   - Check for --test-dir flag (optional)

2. **Generate feature name:**
   - Clean feature name (lowercase, hyphens, no special chars)
   - Example: "User Login Form" → "user-login-form"

3. **Create workspace directory:**
   ```bash
   mkdir -p .claude/playwright-workflow/[feature-name]
   mkdir -p .claude/playwright-workflow/[feature-name]/iterations
   ```

4. **Detect or set test directory:**

   **If --test-dir flag provided:**
   ```
   Use specified directory: [value from flag]
   Verify it exists, create if not
   ```

   **Else, auto-detect:**
   ```
   Priority order:
   1. Check playwright.config.ts for testDir setting
   2. Look for existing directories:
      - tests/
      - e2e/  
      - __tests__/
      - test/
   3. Default: tests/ (create if doesn't exist)
   ```

   **Auto-detection code:**
   ```bash
   # Check playwright config
   if [ -f "playwright.config.ts" ]; then
     TEST_DIR=$(grep "testDir:" playwright.config.ts | sed "s/.*testDir: *['\"]\\([^'\"]*\\).*/\\1/")
   fi
   
   # Fall back to common directories
   if [ -z "$TEST_DIR" ]; then
     for dir in tests e2e __tests__ test; do
       if [ -d "$dir" ]; then
         TEST_DIR="$dir"
         break
       fi
     done
   fi
   
   # Default
   if [ -z "$TEST_DIR" ]; then
     TEST_DIR="tests"
     mkdir -p "$TEST_DIR"
   fi
   
   echo "Test directory: $TEST_DIR"
   ```

5. **Save configuration:**
   ```bash
   # Save to workspace config for later steps
   echo "TEST_DIR=$TEST_DIR" > [workspace]/.config
   echo "FEATURE_NAME=[feature-name]" >> [workspace]/.config
   ```

6. **Verify playwright setup exists:**
   ```bash
   # Check for playwright installation
   if ! npx playwright --version &> /dev/null; then
     echo "⚠️  Warning: Playwright not installed"
     echo "Run: npm install -D @playwright/test"
     echo "Then: npx playwright install"
     exit 1
   fi
   ```

**Output message:**
```
🚀 Initializing Playwright Test Workflow

Feature: [original feature description]
Workspace: .claude/playwright-workflow/[feature-name]/
Test Directory: [detected or specified test directory]
Test File: [test-dir]/[feature-name].spec.ts

✅ Playwright setup detected
✅ Test directory ready

All workflow artifacts will be saved to workspace.
Test file will be created in: [test-dir]/

Starting Requirements Analysis...
```

---

### Step 1: Requirements Analysis

Analyze the feature description and create a comprehensive requirements document.

**Your tasks:**
1. Parse the feature description from $ARGUMENTS
2. Extract or infer the target URL (check for --url flag)
3. Identify the core functionality to test
4. Define test scenarios:
   - Happy path (primary success scenario)
   - Error cases (validation failures, invalid inputs)
   - Edge cases (boundary conditions, empty states)
   - State variations (if applicable)
5. List any assumptions or clarifications needed

**Output:** Save requirements analysis to `[workspace]/requirements.md`

**Format:**
```markdown
# Requirements Analysis

## Feature
[Feature name/description]

## Target URL
[URL to test]

## Core Functionality
[What this feature does]

## Test Scenarios

### 1. Happy Path: [Scenario Name]
**Goal:** [What we're testing]
**Pre-conditions:** [Starting state]
**Expected outcome:** [Success criteria]

### 2. Error Case: [Scenario Name]
[Similar format]

### 3. Edge Case: [Scenario Name]
[Similar format]

## Assumptions
- [List any assumptions made]

## Questions for Clarification
- [Any ambiguities that need user input]
```

**After saving:** Show the user the requirements and ask if they need any adjustments before proceeding to page analysis.

---

### Step 2: Initial Page Analysis

**IMPORTANT:** Analyze the page FIRST, before test planning, but FOCUS on requirements.

**Purpose:** Actively explore the flows mentioned in requirements to give Test Planner real behavioral data.

**Instructions:**
```
Use the playwright-page-analyzer subagent to analyze the page.

Input for subagent:
- Mode: DISCOVERY (requirement-driven exploration)
- Workspace: [workspace-path]
- Requirements: [workspace]/requirements.md (read this first!)
- Target URL: [extracted from requirements]

The subagent will:
1. Read requirements to understand what needs testing
2. Open the actual page with Playwright
3. **ACTIVELY EXPLORE each scenario mentioned in requirements:**
   - If requirement says "test login", actually try logging in
   - If requirement says "test validation", actually trigger validation
   - If requirement says "test errors", actually cause errors
4. Document what ACTUALLY happens (not assumptions)
5. Measure timing, observe behaviors, find edge cases
6. Save complete findings to [workspace]/page-analysis.md

This is ACTIVE EXPLORATION guided by requirements, not passive cataloging.
The goal: Give Test Planner verified, behavioral data about the flows we need to test.

Wait for the subagent to complete before proceeding.
```

**After subagent completes:** Read `[workspace]/page-analysis.md` and show the user:
```markdown
✅ Page Analysis Complete (Requirement-Driven Exploration)

**Requirements Explored:**
- [Scenario 1]: ✅ Tested successfully
- [Scenario 2]: ✅ Tested successfully  
- [Scenario 3]: ⚠️ Discovered edge case

**Flows Verified:**
- [Flow 1]: Confirmed working, timing measured
- [Flow 2]: Confirmed with edge case discovered

**Key Findings:**
- [Finding 1: e.g., "Login takes 780ms, uses /api/auth/login"]
- [Finding 2: e.g., "Validation triggers on blur, not input"]
- [Finding 3: e.g., "Rate limiting after 5 failures"]

**Interactive Elements Found:**
- Buttons: [count relevant to requirements]
- Inputs: [count relevant to requirements]
- [Only elements touched during exploration]

**Measurements Taken:**
- [X] timing measurements
- [Y] behaviors verified
- [Z] edge cases discovered

Full analysis with tested flows saved to: [workspace]/page-analysis.md

✅ Test Planner now has real behavioral data (not assumptions)

Proceeding to test planning with verified flows...
```

---

### Step 3: Test Planning & Validation Loop

**Now that we know what's on the page,** create test plans that match reality.

**Loop Structure:**
```
Iteration 1: Test Planner creates plan (informed by page analysis)
           ↓
           Page Analyzer validates (can I map these steps?)
           ↓
           All steps mappable? → YES → Proceed to Test Writer
           ↓ NO
Iteration 2: Test Planner revises based on validation feedback
           ↓
           Page Analyzer validates again
           ↓
           All steps mappable? → YES → Proceed to Test Writer
           ↓ NO
Iteration 3: Test Planner final revision
           ↓
           Page Analyzer validates
           ↓
           Still unmappable? → Show warning, proceed with best effort
```

**Maximum iterations:** 3

**Your orchestration:**

```
Loop iteration = 1

While iteration <= 3:
  
  # STEP 3A: Test Planner creates/revises plan
  
  Your tasks:
    1. Read [workspace]/requirements.md
    2. Read [workspace]/page-analysis.md (complete page knowledge)
    3. For EACH test scenario in requirements:
       - Design test steps using ACTUAL elements from page analysis
       - Reference specific buttons, inputs, links by their real names
       - Follow flows documented in page analysis
       - Create realistic, mappable steps
    4. Save plan to [workspace]/test-plan.md (version {iteration})
  
  Show user:
    "📝 Test Plan Created (version {iteration})"
    "- Test cases: {N}"
    "- Total steps: {M}"
    "- Based on: page-analysis.md"
    ""
    "Validating plan with Page Analyzer..."
  
  # STEP 3B: Page Analyzer validates plan
  
  Use playwright-page-analyzer subagent:
    Input:
      - Mode: VALIDATION (validating test plan)
      - Workspace: [workspace-path]
      - Test plan: [workspace]/test-plan.md
      - Page knowledge: Already has it from initial analysis
    
    Subagent will:
      - Read test plan
      - Check each step against known page elements
      - Determine if each step is mappable
      - Report validation status
      - Update [workspace]/page-analysis.md with validation results
  
  # STEP 3C: Check validation results
  
  Read [workspace]/page-analysis.md (validation section)
  
  Check validation status:
  
  IF status == ALL_MAPPED:
    Show user: 
      "✅ Validation Complete (iteration {iteration})"
      "All {M} steps successfully mappable!"
      ""
      "Plan approved, proceeding to Test Writer..."
    
    Break loop → Proceed to Test Writer
  
  ELSE IF status == PARTIAL:
    unmappable_count = [count from analysis]
    
    Show user:
      "⚠️  Validation Failed (iteration {iteration}/3)"
      "{unmappable_count} steps cannot be mapped"
      ""
      "Issues:"
      [List each unmappable step with reason from analysis]
    
    IF iteration < 3:
      Show user:
        ""
        "🔄 Revising test plan based on feedback..."
      
      # Loop continues - Test Planner will revise in next iteration
      iteration = iteration + 1
      Continue loop
    
    ELSE (iteration == 3):
      Show user:
        ""
        "⚠️  Warning: After 3 iterations, {unmappable_count} steps still unmappable"
        ""
        "Options:"
        "1. Proceed with partial plan (Test Writer will do best effort)"
        "2. Simplify test scenarios"
        "3. Manual review needed"
        ""
        "How would you like to proceed?"
      
      [Wait for user decision]
      Break loop

End loop
```

**After loop completes:** Show summary and proceed to Test Writer.

**Summary to show:**
```markdown
## Test Planning Complete

**Iterations Required:** {N}
**Final Status:** {ALL_MAPPED / PARTIAL}

**Files:**
- Requirements: [workspace]/requirements.md
- Page Analysis: [workspace]/page-analysis.md
- Test Plan: [workspace]/test-plan.md (version {N})

**Results:**
- ✅ Test cases: {X}
- ✅ Mappable steps: {Y}
- ⚠️  Unmappable steps: {Z} (if any)

**Plan Evolution:**
{if iteration > 1}
- Iteration 1→2: [What changed]
- Iteration 2→3: [What changed]

Ready for test code generation...
```

---

### Step 4: Test Writing

**Format:**
```markdown
# Test Plan

## Test Setup
- Start URL: [URL]
- Pre-conditions: [Any setup needed]
- Test data: [Required test data]

---

## Test Case 1: [Scenario Name]

### Steps

**Step 1:** Navigate to [page]
- Expected: [Expected state after navigation]

**Step 2:** Enter [value] in [field description]
- Input value: "[actual value]"
- Expected: [Expected state after input]

**Step 3:** Click [button/link description]
- Expected: [Expected state after click]

**Step 4:** Verify [element/condition]
- Expected: [What should be visible/true]

### Success Criteria
- [Overall success criteria]

### Edge Cases to Consider
- [Any edge cases specific to this test]

---

## Test Case 2: [Next Scenario]
[Repeat format]
```

**After saving:** Show the user the test plan summary and confirm before proceeding to page analysis.

---

### Step 3: Page Analysis

**IMPORTANT:** Use the `playwright-page-analyzer` subagent for this step.

Delegate to the page analyzer subagent to map the test plan to actual page implementation.

**Instructions:**
```
Use the playwright-page-analyzer subagent to analyze the page.

Input for subagent:
- Workspace: [workspace-path]
- Test plan file: [workspace]/test-plan.md
- Target URL: [extracted from requirements]

The subagent will:
1. Read the test plan from the workspace
2. Open the actual page with Playwright
3. Map each plan step to Playwright selectors and code
4. Document behaviors, timing, and edge cases
5. Save results to [workspace]/page-analysis.md

Wait for the subagent to complete before proceeding.
```

**After subagent completes:** Read `[workspace]/page-analysis.md` and show the user a summary of:
- How many steps were successfully mapped
- Any steps that couldn't be mapped (if any)
- Notable edge cases discovered
- Ask if user wants to refine the plan based on findings

---

### Step 4: Test Writing

Based on the page analysis, write production-ready Playwright test code.

**Your tasks:**
1. Read `[workspace]/page-analysis.md`
2. Read `[workspace]/test-plan.md`
3. Create test file: `[workspace]/[feature-name].spec.ts`
4. Write complete, runnable Playwright tests

**Playwright Best Practices (CRITICAL):**
- ✅ Use semantic selectors: `getByRole`, `getByLabel`, `getByTestId`
- ✅ Use `expect().toBeVisible()` instead of `waitForTimeout()`
- ✅ One clear purpose per test case
- ✅ Tests must be isolated and parallel-safe
- ❌ NO arbitrary `waitForTimeout()` - wait for observable state
- ❌ NO CSS selectors unless absolutely necessary
- ❌ NO overly complex tests (max 20 lines per test)

**Test Structure:**
```typescript
import { test, expect } from '@playwright/test';

test.describe('[Feature Name]', () => {
  
  test('should [scenario description]', async ({ page }) => {
    // Step 1: Navigate
    await page.goto('[url]');
    
    // Step 2-N: Actions based on page-analysis.md
    // Use exact selectors from page analysis
    
    // Assertions
    await expect([selector]).toBeVisible();
    expect(page.url()).toContain('[expected]');
  });
  
  test('should handle [error case]', async ({ page }) => {
    // Similar structure for error cases
  });
  
});
```

**After writing:** Show the user the generated test file and ask if they want to proceed to test execution.

---

### Step 5: Test Execution

**IMPORTANT:** Use the `playwright-test-runner` subagent for this step.

Delegate to the test runner subagent to execute and validate the tests.

**Instructions:**
```
Use the playwright-test-runner subagent to run the tests.

Input for subagent:
- Workspace: [workspace-path]
- Test file: [workspace]/[feature-name].spec.ts
- Max iterations: 2 (for minor fixes only)

The subagent will:
1. Run the Playwright tests
2. Capture results, screenshots on failure
3. Analyze failures and determine if fixable
4. Save results to [workspace]/test-execution.md

If minor failures occur, the subagent will work with you to fix them.
If major failures occur, we'll need to revisit the test plan.

Wait for the subagent to complete before proceeding.
```

**After subagent completes:** Read `[workspace]/test-execution.md` and report results to user.

---

### Step 6: Final Review and Delivery

Present the complete test suite to the user.

**Your tasks:**
1. Read all report files
2. Create a comprehensive summary
3. Provide next steps

**Summary Format:**

```markdown
# Playwright Test Creation - Complete ✅

## Test File Created
- Location: `tests/[feature-name].spec.ts`
- Test cases: [number]
- Total assertions: [number]

## Test Execution Results
- ✅ Passed: [number]
- ❌ Failed: [number]
- ⏱️  Execution time: [time]

## Test Scenarios Covered
1. [Scenario 1] - ✅ Passing
2. [Scenario 2] - ✅ Passing
3. [Scenario 3] - ❌ Failed (reason)

## Files Generated
- `tests/[feature-name].spec.ts` - Test file
- `.claude/test-reports/requirements.md` - Requirements analysis
- `.claude/test-reports/test-plan.md` - Test plan
- `.claude/test-reports/page-analysis.md` - Page analysis
- `.claude/test-reports/test-execution.md` - Execution report

## Next Steps
1. Review the test file: `tests/[feature-name].spec.ts`
2. Review any failures in `.claude/test-reports/test-execution.md`
3. Run tests manually: `npx playwright test tests/[feature-name].spec.ts`
4. Commit when satisfied: `git add . && git commit -m "test: add [feature] E2E tests"`

## Options
- Type "fix [issue]" to address specific test failures
- Type "add scenario [description]" to add more test cases
- Type "refactor tests" to improve test structure
- Type "commit" to commit the changes
```

**Offer:** Ask the user if they want to:
- Fix any failing tests
- Add more scenarios
- Refactor/improve the tests
- Commit the changes
- Start over with different requirements

---

## Error Handling

If any step fails:
1. Save error details to `[workspace]/error.log`
2. Explain what went wrong
3. Offer options:
   - Retry the failed step
   - Skip to next step (if possible)
   - Restart from beginning
   - Exit workflow (workspace preserved for debugging)

## Workspace Management

**Workspace persistence:**
- Each workflow creates a new workspace under `.claude/playwright-workflow/[feature-name]/`
- Workspaces are preserved until explicitly cleaned up
- Multiple workflows can run for different features simultaneously
- Workspace contains complete audit trail of test creation process

**Workspace cleanup:**
After successful test creation and when user is satisfied:
```bash
# Archive workspace
mkdir -p .claude/playwright-workflow/.archive
mv .claude/playwright-workflow/[feature-name] .claude/playwright-workflow/.archive/[feature-name]-$(date +%Y%m%d)

# Or delete workspace
rm -rf .claude/playwright-workflow/[feature-name]
```

**Workspace benefits:**
- ✅ Complete isolation per feature
- ✅ Easy to review entire workflow history
- ✅ Safe to iterate without polluting main directories
- ✅ Multiple parallel workflows possible
- ✅ Easy cleanup after completion

## Notes

- All reports are saved in feature-specific workspace for transparency and debugging
- User can review any report file at any time
- Subagents work in isolated contexts but communicate via workspace files
- Maximum 2 iterations for test fixes (prevents infinite loops)
- If tests still fail after 2 iterations, recommend manual review
- Test file only moves to `tests/` directory after all tests pass
- Workspace can be archived or cleaned up after successful completion