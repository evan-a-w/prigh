if vim.g.loaded_prigh then
  return
end
vim.g.loaded_prigh = true

-- Commands work without setup(); setup() adds the keymaps and options.
require("prigh").create_commands()
