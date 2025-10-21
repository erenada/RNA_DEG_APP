# Experimental Design Tab Documentation

**Author:** Eren Ada, PhD  
**Last Updated:** October 21, 2025  
**Module Files:** `ui/tab_design.R`, `server_modules/server_tab2_experimental_design.R`

---

## Table of Contents

1. [Overview](#overview)
2. [Key Features](#key-features)
3. [User Interface](#user-interface)
4. [Drag-and-Drop System](#drag-and-drop-system)
5. [Covariate System](#covariate-system)
6. [User Workflow](#user-workflow)
7. [Design Validation](#design-validation)
8. [Contrast Import/Export](#contrast-importexport)
9. [Troubleshooting](#troubleshooting)

---

## Overview

The **Experimental Design** tab enables users to create contrasts for differential expression analysis using an intuitive drag-and-drop interface. Users assign samples to comparison groups (numerator vs denominator), optionally add covariates, and generate valid DESeq2 design formulas.

### Purpose

- Create pairwise comparisons between experimental groups
- Configure additive covariates for batch correction or multi-factor designs
- Validate design matrices for rank deficiency
- Import/export contrast definitions

### Design Philosophy

**Numerator vs Denominator:** DESeq2 calculates log2FC as numerator/denominator. Positive log2FC indicates upregulation in the numerator group.

**Approach:** User-defined grouping with optional additive covariates. The factor of interest is always placed last in the design formula.

---

## Key Features

### Sample Management
- Interactive drag-and-drop sample assignment
- Metadata-based filtering of available samples
- Visual grouping with color-coded containers
- Sample metadata tooltips on hover

### Contrast Creation
- User-defined group names with validation
- Custom factor naming for design formula
- Real-time contrast preview
- Design formula visualization

### Covariate Support
- Add up to 4 additive covariates
- Automatic detection of categorical vs numeric
- Reference level control for categorical variables
- Design validation and warning system

### Quality Control
- Name sanitization and validation
- Rank deficiency detection
- Confounding detection
- Low replication warnings

---

## User Interface

### Layout Structure

```
┌─────────────────────────────────────────────────┐
│  Design Notes (Best Practices Info Box)        │
├──────────────┬──────────────────────────────────┤
│  Available   │  Comparison Groups               │
│  Samples     │  ┌─────────┬──────────┐         │
│  ├─Filters   │  │ Group 1 │ Group 2  │         │
│  ├─List      │  │ (Numer) │ (Denom)  │         │
│  └─Actions   │  └─────────┴──────────┘         │
│              │  Contrast Preview               │
│              │  Advanced: Add Covariates       │
│              │  Create Contrast Button         │
├──────────────┴──────────────────────────────────┤
│  Contrast Management (Import/Export)           │
├─────────────────────────────────────────────────┤
│  Created Contrasts List                        │
│  Navigation Buttons                            │
└─────────────────────────────────────────────────┘
```

### Sample Filtering Panel

**Location:** Left sidebar  
**Components:**
- Dynamic filters based on metadata columns
- "Show All" button to reset filters
- Sample count display
- Filtered sample list (draggable)

**Code Reference:**
```69:94:server_modules/server_tab2_experimental_design.R
output$metadata_filters <- renderUI({...})
update_filtered_samples <- function() {...}
```

### Comparison Groups Panel

**Location:** Right main area  
**Components:**

1. **Focus Group (Numerator)**
   - Green border
   - Group name input
   - Drop zone for samples
   - Helper text with tooltips

2. **Background/Control Group (Denominator)**
   - Orange border
   - Group name input
   - Drop zone for samples
   - Reference level guidance

**Group Name Validation:**
- Must start with letter
- Can contain letters, digits, underscore
- Validated in real-time (lines 29-48)

### Contrast Preview Area

**Displays:**
- Group names and sample counts
- Readiness status ("Ready to create contrast!")
- Design approach (user-defined with/without covariates)
- Design formula preview
- Contrast specification

**Code Reference:**
```500:562:server_modules/server_tab2_experimental_design.R
output$contrast_preview <- renderUI({...})
```

### Advanced: Add Covariates Panel

**Collapsible Section**  
**Inputs:**
- Covariate selection (multi-select, max 4)
- Reference level controls (categorical only)
- Reset button

**Dynamic Updates:**
- Automatically excludes factor name
- Excludes sample ID columns
- Excludes all-unique columns
- Updates when groups change

**Code Reference:**
```320:352:server_modules/server_tab2_experimental_design.R
observe({ # Update covariate choices
```

---

## Drag-and-Drop System

### Implementation

**JavaScript File:** `www/js/design_drag_drop.js`  
**Communication:** Custom Shiny input `input$sample_moved`

### Event Flow

```
User drags sample → dragstart event → store sample ID
User drops sample → drop event → identify target zone
                 → send to Shiny → update reactive values
                 → re-render UI lists
```

### Sample Movement Handler

**Code Reference:**
```291:313:server_modules/server_tab2_experimental_design.R
observeEvent(input$sample_moved, {
  # Remove from all lists
  # Add to target zone
  # Update filtered samples
})
```

**Zones:**
- `available_samples`: Return to pool
- `group1_samples`: Add to numerator
- `group2_samples`: Add to denominator

### Visual Feedback

**CSS Classes:**
- `.sample-item`: Draggable sample badges
- `.drop-zone`: Drop target areas
- `.dragover`: Active drop zone highlighting

**Hover Effects:**
- Sample tooltips show metadata
- Drop zones change background when dragged over

---

## Covariate System

### Purpose

Covariates allow adjustment for:
- Batch effects
- Time points
- Subject/pairing effects
- Other experimental factors

**Design Formula:** `~ covariate1 + covariate2 + factor_of_interest`

### Covariate Selection Logic

**Auto-Exclusions:**
```330:349:server_modules/server_tab2_experimental_design.R
# Exclude factor name
# Exclude sample ID columns
# Exclude all-unique columns (likely IDs)
```

### Categorical Covariates

**Reference Level Control:**
- User selects reference level via dropdown
- First level becomes baseline in model
- Applied during design matrix creation

**Code Reference:**
```355:413:server_modules/server_tab2_experimental_design.R
output$covariate_reference_controls <- renderUI({...})
```

### Numeric Covariates

**Treatment:**
- Coerced to numeric
- Included as-is in formula
- Variance checked (warning if near-zero)

**Code Reference:**
```676:684:server_modules/server_tab2_experimental_design.R
# Numeric: coerce
md_enhanced[[cv]] <- as.numeric(md_enhanced[[cv]])
if (var(md_enhanced[[cv]], na.rm=TRUE) < 1e-6) {
  showNotification(...)
}
```

### Design Formula Preview

**Dynamic Updates:**
- Rebuilds formula on covariate changes
- Shows complete design formula
- Validates terms order (covariates → factor)

**Code Reference:**
```416:444:server_modules/server_tab2_experimental_design.R
output$design_formula_preview <- renderUI({...})
```

---

## User Workflow

### Step 1: Filter and Select Samples

1. Review available samples (matched from Data Input tab)
2. Apply metadata filters to narrow selection (optional)
3. Sample count updates dynamically

### Step 2: Drag Samples to Groups

1. Drag samples from "Available Samples" to comparison groups
2. Drag to **Group 1 (Numerator)**: Treatment/condition of interest
3. Drag to **Group 2 (Denominator)**: Control/reference group
4. Samples can be dragged back to available pool or between groups

**Best Practice:** Aim for 3+ biological replicates per group

### Step 3: Name Your Groups

1. Enter descriptive group names (e.g., "Treatment", "Control")
2. Names must be valid R identifiers (start with letter)
3. Real-time validation with error notifications
4. Optional: Customize factor name (default: "contrast_group")

### Step 4: Add Covariates (Optional)

1. Click "Advanced: Add Covariates"
2. Select metadata columns to adjust for
3. Set reference levels for categorical covariates
4. Review design formula preview
5. Check validation warnings

### Step 5: Create Contrast

1. Review contrast preview (group names, sample counts, formula)
2. Click "Create Contrast" button (enabled when valid)
3. Contrast appears in "Created Contrasts" list
4. Groups clear automatically for next contrast

### Step 6: Manage Contrasts

- Review created contrasts with collapsible details
- Remove individual contrasts if needed
- Export all contrasts to CSV for reproducibility
- Import previously saved contrasts

### Step 7: Proceed to Analysis

1. Ensure at least one contrast created
2. Click "Next: Analysis Configuration"
3. All contrast metadata passed to DESeq2 analysis

---

## Design Validation

### Name Validation

**Function:** `is_valid_identifier()`  
**Rules:**
- Must start with letter
- Can contain: letters, digits, underscore
- No spaces, special characters, or punctuation

**Sanitization:** `sanitize_identifier()` auto-corrects when creating design (lines 11-20)

### Covariate Validation

**Checks:**

1. **Single-level categorical** (Error)
   - ≤1 unique value among contrast samples
   - Cannot model single-level factors
   - Solution: Remove covariate or add samples

2. **Near-zero variance numeric** (Warning)
   - Variance < 1e-6
   - Minimal contribution to model
   - May cause numerical issues

**Code Reference:**
```460:492:server_modules/server_tab2_experimental_design.R
output$design_warnings <- renderUI({...})
```

### Rank Deficiency Detection

**Function:** `detect_rank_issue()`  
**Purpose:** Identify why design matrix is not full rank

**Checks:**
```578:593:server_modules/server_tab2_experimental_design.R
# Check for perfect confounding
# Contingency table analysis
# Detect linear dependencies
```

**Common Causes:**
- Covariate perfectly confounded with contrast groups
- Empty factor levels after subsetting
- Insufficient sample diversity

**Handling:**
```694:704:server_modules/server_tab2_experimental_design.R
X <- model.matrix(design_formula, md_enhanced)
rank_deficient <- qr(X)$rank < ncol(X)
if (rank_deficient) {
  error_msg <- detect_rank_issue(...)
  stop(error_msg)
}
```

### Level Dropping

**Critical Step:** Empty factor levels are dropped to avoid all-zero columns in model matrix.

**Code Reference:**
```646:661:server_modules/server_tab2_experimental_design.R
# Drop "Unused" level from factor_of_interest
if (is.factor(md_enhanced[[factor_name]])) {
  md_enhanced[[factor_name]] <- droplevels(md_enhanced[[factor_name]])
}

# Drop empty levels from covariates
for (cv in covariates) {
  if (!is.numeric(md_enhanced[[cv]])) {
    md_enhanced[[cv]] <- droplevels(factor(md_enhanced[[cv]]))
  }
}
```

---

## Contrast Import/Export

### Export Contrasts to CSV

**Functionality:**
- Exports all created contrasts
- Includes group samples, formula, covariates
- Timestamped filename

**CSV Structure:**
```csv
contrast_name,group1_samples,group2_samples,design_formula,covariates
Treatment_vs_Control,"Sample1,Sample2","Sample3,Sample4",~ batch + contrast_group,batch
```

**Code Reference:**
```1066:1143:server_modules/server_tab2_experimental_design.R
output$export_contrasts <- downloadHandler({...})
```

### Import Contrasts from CSV

**Functionality:**
- Validates sample names against metadata
- Reconstructs contrast objects
- Checks for sample overlap
- Recreates enhanced metadata

**Validation:**
- Required columns: `contrast_name`, `group1_samples`, `group2_samples`
- Sample names must exist in uploaded metadata
- No sample overlap between groups

**Code Reference:**
```1146:1332:server_modules/server_tab2_experimental_design.R
observeEvent(input$import_contrasts_file, {...})
```

**Use Case:** Share analysis designs between projects or team members

---

## Troubleshooting

### "Group name invalid" error

**Cause:** Group name violates R identifier rules

**Solution:**
- Start with a letter (not number or underscore)
- Use only letters, digits, underscore
- Avoid spaces and special characters
- Example valid names: `Treatment`, `Day_0`, `Batch1`

### "Design is not full rank" error

**Cause:** Model matrix is rank deficient (linear dependencies)

**Common Scenarios:**

1. **Perfect confounding:**
   - All Group1 samples are Batch1, all Group2 are Batch2
   - Solution: Remove batch covariate or use different grouping

2. **Empty factor levels:**
   - Rare; usually auto-handled by `droplevels()`
   - Solution: Verify all factor levels have samples

**Diagnostic Steps:**
1. Check covariate distribution across groups
2. Remove covariates one-by-one to identify culprit
3. Simplify to single-factor design first

### "Covariate has ≤1 level" error

**Cause:** Categorical covariate has same value for all contrast samples

**Solution:**
- Remove the covariate (cannot model constant)
- Add samples with different levels
- Choose different samples for contrast

### Samples not appearing in available list

**Cause:** Metadata filters are active

**Solution:**
- Click "Show All Samples" to reset filters
- Adjust filter dropdowns to include desired samples

### Drag-and-drop not working

**Possible Causes:**
- JavaScript not loaded
- Browser compatibility issue

**Solution:**
- Refresh browser
- Check browser console for errors
- Verify `www/js/design_drag_drop.js` is loaded

### Created contrast missing samples

**Cause:** Only contrast samples are used (others marked "Unused")

**Expected Behavior:**
- Samples not in either group are excluded from that contrast
- Each contrast can use different sample subsets
- "Unused" level in enhanced metadata for non-contrast samples

---

## Advanced Topics

### Enhanced Metadata Structure

Each contrast creates an enhanced metadata object:

```r
enhanced_metadata <- values$meta_data  # Start with original
enhanced_metadata[[factor_name]] <- factor(
  contrast_factor,
  levels = c(group2_name, group1_name, "Unused")
)
```

**Key Points:**
- Factor levels ordered: reference first, then numerator
- Non-contrast samples labeled "Unused"
- Passed to DESeq2 for analysis

### Contrast Object Structure

```r
values$created_contrasts[[contrast_name]] <- list(
  name = "Treatment_vs_Control",
  group1_name = "Treatment",
  group2_name = "Control",
  group1_samples = c("Sample1", "Sample2"),
  group2_samples = c("Sample3", "Sample4"),
  design_analysis = list(
    design_strategy = list(
      approach = "user_defined_additive",
      design_formula = "~ batch + contrast_group",
      covariates = c("batch"),
      factor_name = "contrast_group",
      contrast_specification = c("contrast_group", "Treatment", "Control"),
      total_samples = 4,
      unused_samples = 2
    ),
    enhanced_metadata = <data.frame>
  ),
  enhanced_metadata = <data.frame>,
  creation_timestamp = <POSIXct>
)
```

### Design Formula Construction

**Order Matters:**
```r
rhs_terms <- c(covariates, factor_name)
design_formula <- as.formula(paste("~", paste(rhs_terms, collapse = " + ")))
```

**Reason:** DESeq2 `results()` extracts the last coefficient by default. Placing the factor of interest last ensures correct results extraction.

### JavaScript Integration

**Custom Input Handler:**
```javascript
Shiny.addCustomMessageHandler('sample_moved', function(data) {
  Shiny.setInputValue('sample_moved', {
    sample: data.sample,
    target: data.target,
    timestamp: new Date().getTime()
  });
});
```

**Benefit:** Enables drag-and-drop without external packages

---

## Best Practices Summary

### Experimental Design

1. **Biological Replicates:** Use ≥3 per group for statistical power
2. **Reference Selection:** Set control/wildtype as denominator (reference)
3. **Balanced Design:** Equal replicates across groups when possible
4. **Avoid Confounding:** Don't let covariates perfectly align with groups

### Covariate Usage

1. **When to Add:**
   - Known batch effects
   - Paired samples (subject/time)
   - Technical confounders

2. **When to Skip:**
   - Single-factor experiments
   - Covariates perfectly confounded with groups
   - Insufficient sample size (covariate uses degrees of freedom)

### Naming Conventions

1. **Group Names:** Descriptive and concise (e.g., `Treated`, `Untreated`)
2. **Factor Names:** Lowercase with underscores (e.g., `treatment_condition`)
3. **Contrast Names:** Auto-generated as `Group1_vs_Group2`

---

## Related Documentation

- **Data Input Tab:** [01_data_input_tab.md](01_data_input_tab.md)
- **Analysis Configuration Tab:** `03_analysis_configuration_tab.md` (coming soon)
- **DESeq2 Utilities:** See `R/utils_deseq2.R` for design implementation

---

## Quick Reference

### Keyboard/Mouse Actions

| Action | Result |
|--------|--------|
| Drag sample | Move between zones |
| Click sample | No action (draggable only) |
| Hover sample | Show metadata tooltip |
| Click filter dropdown | Filter available samples |
| Click "Show All" | Reset all filters |

### Button Functions

| Button | Action |
|--------|--------|
| Create Contrast | Generate contrast from current groups |
| Clear All | Return all samples to available pool |
| Reset to Simple Design | Remove all covariates |
| Show All Samples | Clear metadata filters |
| Remove (per contrast) | Delete specific contrast |
| Export Contrasts | Download CSV of all contrasts |
| Import Contrasts | Upload CSV to load contrasts |

### Status Indicators

| Color | Meaning |
|-------|---------|
| Green border | Numerator group (focus) |
| Orange border | Denominator group (reference) |
| Blue text | Ready to create |
| Yellow warning | Validation warnings |
| Red error | Invalid design |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-10-21 | Initial documentation created |

---

**End of Experimental Design Tab Documentation**

