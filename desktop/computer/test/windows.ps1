# prigh-computer on a real Windows desktop (the CI's Windows runner): the
# helper probes the screen, moves the pointer and reads it back, then types
# into Notepad, selects and copies it, and the clipboard must hold what was
# typed (Unicode included, so KEYEVENTF_UNICODE and the key combos both
# worked).
#
#   pwsh desktop/computer/test/windows.ps1 PATH\TO\prigh-computer.exe
param([Parameter(Mandatory)][string]$Helper)
$ErrorActionPreference = 'Stop'
$utf8 = [System.Text.UTF8Encoding]::new($false)
$OutputEncoding = $utf8
[Console]::OutputEncoding = $utf8

function Strip($line) {
    $line -replace '"data":"[^"]*"', '"data":"…"'
}

Write-Output '== probe'
& $Helper --probe
if ($LASTEXITCODE -ne 0) { throw "prigh-computer --probe failed ($LASTEXITCODE)" }

$notepad = Start-Process notepad -PassThru
try {
    $deadline = (Get-Date).AddSeconds(30)
    while ($notepad.MainWindowHandle -eq 0) {
        if ((Get-Date) -gt $deadline) { throw 'Notepad never showed a window' }
        Start-Sleep -Milliseconds 100
        $notepad.Refresh()
    }
    Set-Clipboard -Value 'not copied'
    $text = 'hello é→ 😀'
    $requests = @(
        '{"id":1,"op":"info"}'
        '{"id":2,"op":"act","action":{"action":"move","x":100,"y":100}}'
        '{"id":3,"op":"act","action":{"action":"cursor_position"}}'
        (@{ id = 4; op = 'act'; action = @{ action = 'type'; text = $text } } | ConvertTo-Json -Compress)
        '{"id":5,"op":"act","action":{"action":"key","combo":"ctrl+a ctrl+c"}}'
    )
    Write-Output '== session'
    $replies = $requests | & $Helper --max-long-side 640 --settle-ms 300
    if ($LASTEXITCODE -ne 0) { throw "prigh-computer failed ($LASTEXITCODE)" }
    $replies | ForEach-Object { Strip $_ }
    $parsed = $replies | ForEach-Object { $_ | ConvertFrom-Json }
    foreach ($r in $parsed) {
        if (-not $r.ok) { throw "request $($r.id) failed: $($r.error)" }
    }
    $cursor = $parsed[2].cursor
    if ($cursor.x -ne 100 -or $cursor.y -ne 100) { throw "the pointer is at ($($cursor.x), $($cursor.y)), not (100, 100)" }
    $copied = Get-Clipboard -Raw
    Write-Output "== clipboard: $copied"
    if ($copied -ne $text) { throw "Notepad got '$copied', not '$text'" }
    Write-Output 'ok'
}
finally {
    Stop-Process -Id $notepad.Id -Force -ErrorAction SilentlyContinue
}
