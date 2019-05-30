See the spec sheet [here](https://docs.google.com/spreadsheets/d/1ZLgES_sVv9S_Sl17oUNYj9-MAC9aaQvMX2tyMW_bHSg/edit?usp=sharing). The goal of this is to enumerate all possible *inputs* (including state variables, arguments, and results of external calls) and list the expected *delta* (state changes, emitted events, etc.)

Generally, the document is split up into one function per sheet (see tabs at bottom), and the naming convention is [phase].[functionName] (so the sheet for `commit`, which has an `inPhase(Phase.Open)` modifier, is on the sheet titled "Open.commit").

For each of these sheets, the section to the left of the blue bar is the *input*, where each column represents a **relevant term**. A **term** is an argument, state variable, a function which will return a result, or an expression containing any of other relevant terms; and a term is **relevant** if it can branch the logic of the function, affect the return value, or trigger a revert. Each sheet should capture all **relevant terms** used in the function.

Each sheet should then enumerate every possible combination of the relevant terms on the left, and to the right, the expected state *delta* is described in terms of its return value, state change, token calls, and emitted events.

Each line thus enumerated is given a **spec code**, and given a character code (red letter in the gray column) indicating the progress or plans for testing that particular case:

- (blank): Manually tested whenever the function code is modified
- E: **E**ssential; a test must be written for this case
- W: Test **W**ritten and passed

The tests we've written can be easily found by searching in src/Tests.t.sol for the case spec code.