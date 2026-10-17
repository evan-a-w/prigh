local M = {}

M.defaults = {
  -- The agent: any ACP agent works; prigh's is `prigh acp`.
  -- Remote backend: { "prigh", "acp", "-connect", "host:7777", "-token", "T" }
  cmd = { "prigh", "acp" },
  env = nil, -- extra environment variables for the agent
  cwd = nil, -- session directory; default: vim.fn.getcwd()
  -- Load the last session of this directory when the chat first opens
  -- (needs the agent's loadSession capability).
  resume_last = true,
  keymaps = {
    -- Global mappings, all under this prefix; `keymaps = false` disables them.
    prefix = "<leader>p",
    toggle = "p",
    new = "n",
    sessions = "s",
    add = "a",
    diagnostics = "d",
    clear_context = "x",
    cancel = "c",
    mode = "m",
    model = "M",
    config = "o",
    review = "r",
    log = "l",
  },
  chat = {
    position = "right", -- right | left | bottom | top
    width = 0.4, -- columns (fraction of the editor or absolute)
    height = 0.4, -- for top/bottom
    input_height = 6,
    -- Tool output lines shown before collapsing (<Tab> expands).
    tool_output_lines = 0,
    -- Diffs longer than this are collapsed in the chat.
    diff_max_lines = 40,
    -- Thoughts are dimmed; collapsed shows only a summary line.
    thoughts_collapsed = true,
  },
  permission = {
    -- Edit previews longer than this are cut in the card; `d` opens them in a float.
    inline_diff_max = 20,
  },
  fs = {
    -- After fs/write_text_file updates a loaded buffer, :write it so the
    -- agent's other tools (shell, compilers) see the change.
    save_after_write = true,
    -- Run BufWritePre/Post autocommands (formatters, linters) on those writes.
    -- Off by default: a formatter would change what the agent just wrote.
    write_autocmds = false,
  },
  log = {
    max_lines = 5000,
    traffic = true, -- log every JSON-RPC message (truncated)
  },
}

M.options = vim.deepcopy(M.defaults)

function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
  if opts and opts.keymaps == false then
    M.options.keymaps = false
  end
  if opts and opts.cmd then
    M.options.cmd = opts.cmd
  end
  return M.options
end

return M
