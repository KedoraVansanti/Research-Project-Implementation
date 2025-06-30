// Global variables
var index = 0;  // Keeps track of the current task (Length or Width)
var next_task = newArray("Length", "Width");  // Order of tasks for alternating between Length and Width
var cur_task = newArray("Length", "Width");  // The current task (Length or Width)
var initialized = false;  // Flag to track if initialization has been completed
var CellID = -1;  // Variable to store the current cell number

// Function to set up the table columns (only once)
function setupTable() {
    Table.setColumn("Cell-Nr");  // Cell number
    Table.setColumn("Task");  // Length or Width
    Table.setColumn("X");  // Midpoint X coordinate
    Table.setColumn("Y");  // Midpoint Y coordinate
    Table.setColumn("Cement [1=No/2=Yes]");  // Cement status (1 = not cemented, 2 = cemented)
}

// Function to measure and label the cells
function measureAndLabel(cementValue) {
    // Initialize only once, set up the Cell ID
    if (!initialized) {
        // Ask the user for the starting Cell ID (default = 1)
        CellID = parseInt(getString("Start Cell ID", "1"));
        
        // Set up the table columns
        setupTable();
        
        // Mark as initialized
        initialized = true;
    }

    // Check if the correct tool (Line) is selected
    if (selectionType != 5) {
        showMessage("Error: incorrect tool", "Please select the 'Line' tool before proceeding.");
        return;
    }

    // Measure the length and angle of the current line
    run("Measure");
    run("Draw");

    // Get the coordinates of the selected line
    getSelectionCoordinates(xPoints, yPoints);
    X = (xPoints[0] + xPoints[1]) / 2;  // Calculate the midpoint X
    Y = (yPoints[0] + yPoints[1]) / 2;  // Calculate the midpoint Y

    // If the task is 'Width', draw the label at the midpoint
    if (cur_task[index] == "Width") {
        drawLabel(X, Y, CellID);  // Call function to draw label with CellID
    }

    // Save the measured data into the results table
    row = nResults - 1;  // Get the last row
    Table.set("Cell-Nr", row, "" + CellID);  // Save the CellID
    Table.set("Task", row, cur_task[index]);  // Save the current task (Length/Width)
    Table.set("X", row, X);  // Save the X coordinate (midpoint)
    Table.set("Y", row, Y);  // Save the Y coordinate (midpoint)
    Table.set("Cement [1=No/2=Yes]", row, "" + cementValue);  // Save the cement value (1 = No, 2 = Yes)

    // Increment CellID after measuring 'Width'
    if (cur_task[index] == "Width") {
        CellID++;  // Increment the cell number after a Width measurement
    }

    // Advance to the next task (Length → Width or vice versa)
    advanceTask();
}

// Function to draw the label (CellID) at the given coordinates
function drawLabel(X, Y, CellID) {
    fontSize = 10;  // Set the font size for the label
    setFont("SansSerif", fontSize, "bold");  // Set font style and size
    text = "" + CellID;  // Convert CellID to string
    textWidth = lengthOf(text) * fontSize * 0.6;  // Calculate width of the text
    textHeight = fontSize + 4;  // Set the height of the text

    // Calculate the rectangle position around the label
    rectX = X - (textWidth / 2) - 4;  // X position adjusted to center the text
    rectY = Y - textHeight + 8;  // Y position adjusted to place the label below the point

    // Draw a white background for the label
    setColor(255, 255, 255);  // Set color to white
    makeRectangle(rectX, rectY, textWidth + 8, textHeight);  // Create the rectangle
    run("Fill");  // Fill the rectangle with the white color

    // Draw the label text in black
    setColor("black");
    drawString(text, X - (textWidth / 2), Y + 8);  // Position the text within the rectangle
}

// Function to advance to the next task
function advanceTask() {
    index = (index + 1) % cur_task.length;  // Switch between Length and Width
    print("Next task: " + next_task[index]);  // Print the next task in the log
}

// Macro for non-cemented Cells 
macro "Measure and Draw for non-cemented Cells [1]" {
    measureAndLabel(1);  // Call measureAndLabel with cement value 1 (not cemented)
}

// Macro for cemented Cells 
macro "Measure and Draw for cemented Cells [2]" {
    measureAndLabel(2);  // Call measureAndLabel with cement value 2 (cemented)
}
