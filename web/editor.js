// A small code editor: a <textarea> over a <pre> showing the same text with
// Python-style highlighting. The textarea's own text is transparent, so
// editing, selection and undo stay native.

const KEYWORDS = new Set([
  "and", "as", "assert", "async", "await", "break", "class", "continue",
  "def", "del", "elif", "else", "except", "finally", "for", "from", "global",
  "if", "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass",
  "raise", "return", "try", "while", "with", "yield",
]);
const CONSTANTS = new Set(["True", "False", "None"]);
const BUILTINS = new Set(["print", "len", "input"]);
const TYPES = new Set(["int", "str", "bool", "object"]);

// One alternative per token kind, tried in order at each position
const TOKEN = /(#.*)|("(?:[^"\\\n]|\\.)*"?)|(\b\d+\b)|([A-Za-z_]\w*)|(\s+|.)/gy;

function escape(text) {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;");
}

function span(kind, text) {
  return `<span class="tok-${kind}">${escape(text)}</span>`;
}

// Returns the HTML for source with each token wrapped in a tok-* span
export function highlight(source) {
  let html = "";
  let previousWord = "";
  TOKEN.lastIndex = 0;
  for (const [text, comment, string, number, word] of source.matchAll(TOKEN)) {
    if (comment) {
      html += span("comment", text);
    } else if (string) {
      html += span("string", text);
    } else if (number) {
      html += span("number", text);
    } else if (word) {
      if (previousWord === "def")
        html += span("function", text);
      else if (previousWord === "class")
        html += span("class", text);
      else if (KEYWORDS.has(word))
        html += span("keyword", text);
      else if (CONSTANTS.has(word))
        html += span("constant", text);
      else if (BUILTINS.has(word))
        html += span("builtin", text);
      else if (TYPES.has(word))
        html += span("type", text);
      else
        html += escape(text);
      previousWord = word;
    } else {
      html += escape(text);
    }
  }
  return html;
}

// Replaces the selection with text, keeping the browser's undo history
function insertText(textarea, text) {
  if (!document.execCommand("insertText", false, text))
    textarea.setRangeText(text, textarea.selectionStart, textarea.selectionEnd, "end");
}

// Turns textarea and the code element of a pre behind it into an editor.
// onRun is called on Ctrl/Cmd+Enter.
export function createEditor(textarea, code, { onRun }) {
  const render = () => {
    // A trailing newline needs content after it to take up a line
    code.innerHTML = highlight(textarea.value) + "\n";
  };
  textarea.addEventListener("input", render);
  textarea.addEventListener("scroll", () => {
    code.parentElement.scrollTop = textarea.scrollTop;
    code.parentElement.scrollLeft = textarea.scrollLeft;
  });

  textarea.addEventListener("keydown", (event) => {
    if (event.key === "Enter" && (event.ctrlKey || event.metaKey)) {
      event.preventDefault();
      onRun();
    } else if (event.key === "Tab" && !event.shiftKey) {
      event.preventDefault();
      insertText(textarea, "    ");
    } else if (event.key === "Enter") {
      // Keep the line's indentation, one level more after a colon
      const before = textarea.value.slice(0, textarea.selectionStart);
      const line = before.slice(before.lastIndexOf("\n") + 1);
      let indent = line.match(/^ */)[0];
      if (/:\s*(#.*)?$/.test(line))
        indent += "    ";
      event.preventDefault();
      insertText(textarea, "\n" + indent);
    }
  });

  render();
  return {
    get value() {
      return textarea.value;
    },
    set value(source) {
      textarea.value = source;
      textarea.scrollTop = 0;
      render();
    },
  };
}
