This is the source for browser extensions for [firefox](https://addons.mozilla.org/fr/firefox/addon/orthographe-simplifi%C3%A9e/) and [chrome](https://chromewebstore.google.com/detail/orthographe-simplifiée/jdicbfmgcajnpealjodkghahiakdafcl). See these pages for what the extensions do.

# How to tweak and try locally

Run:

```bash
./import-dict
./make-icon
```

Or, alternatively with `dune`:

```bash
# (cd ../; make install-opam-and-dune)
# dune build
```

For firefox: the first time, go to `about:debugging` -> `this firefox`
-> `load a temporary module`, point to
`extension/src/manifest.json`, go to `about:addons`, click on
the extension, then `permissions` and give the permission to access
on sites. On future changes, press `refresh` instead.

For firefox with manifest v2 (as an easier step than checking on
android), run `./make-extension` and load
`extension/src2/manifest.json`.  On future changes, press `refresh`
instead.

For chrome: the first time, go to `chrome://extensions` -> `load
unpacked extension` and point to `extension/src`. Afterwards, press
the refresh icon instead.

# A few implementation notes

## manifest.json
### Why `gecko_id`
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/browser_specific_settings

### Why storage permissions
For the options

### Why `min_version` of 113 for android
106: needed for `i18.detectLanguage`
113: needed for `gecko_android.min_version`

### Why `manifest_version` 2:

Because with manifest v3, firefox will not run the extension unless
the user either interacts with the extension, allows the extension to
run on the page, or goes into the extension settings to allow it to
run on all pages. On android, there is no UI to do the latter, making
the extension unusable.

With manifest v2, the user is prompted at installation time and then
the extension just works.

More context:
https://stackoverflow.com/questions/77354847/how-to-run-a-content-script-without-user-click-on-webextension-v3

Note that this problem is firefox-specific. Chrome just runs the
extension. So we keep using v3 for chrome, as they intend to deprecate
v2.

Another problem is that with v3, firefox refuses to run any extension
on file:// url. Again, this works properly with v2, or v3 in chrome.
