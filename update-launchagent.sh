#! /bin/bash

LIBDIR="${HOME}/Library"
LOGDIR="${LIBDIR}/Logs/Emacs"
LAUNCHDIR="${LIBDIR}/LaunchAgents"
PLIST="${LAUNCHDIR}/com.scottylogan.emacs.plist"
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

mkdir -p "${LOGDIR}"

launchctl unload "${PLIST}" >/dev/null 2>&1

cat >"${PLIST}" <<XML
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
    <dict>
        <key>Label</key>
        <string>com.scottylogan.emacs</string>
        <key>Disabled</key>
        <false/>
        <key>EnvironmentVariables</key>
        <dict>
            <key>PATH</key>
            <string>${PATH}</string>
        </dict>
        <key>ProgramArguments</key>
        <array>
            <string>${EMACS}</string>
            <string>-nw</string>
            <string>--fg-daemon=${USER}</string>
        </array>
        <key>ProcessType</key>
        <string>Interactive</string>
        <key>KeepAlive</key>
        <true/>
        <key>RunAtLoad</key>
        <true/>
        <key>StandardOutPath</key>
        <string>${LOGDIR}/server.log</string>
        <key>StandardErrorPath</key>
        <string>${LOGDIR}/error.log</string>
    </dict>
</plist>
XML

launchctl load "${PLIST}"
