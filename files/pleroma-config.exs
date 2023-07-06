# Pleroma instance configuration
import Config

config :pleroma, Pleroma.Web.Endpoint,
   url: [host: "alt.moron.city", scheme: "https", port: 443],
   http: [ip: {0, 0, 0, 0}, port: 4000]

config :pleroma, :instance,
  name: "alt.moron.city",
  email: "edd@eddsteel.com",
  notify_email: "edd@eddsteel.com",
  limit: 5000,
  registrations_open: true

config :pleroma, :media_proxy,
  enabled: false,
  redirect_on_failure: true
  #base_url: "https://cache.pleroma.social"

config :pleroma, :database, rum_enabled: false
config :pleroma, :instance, static_dir: "/var/lib/pleroma/static"

# Configure S3 support if desired.
# The public S3 endpoint (base_url) is different depending on region and provider,
# consult your S3 provider's documentation for details on what to use.
#
 config :pleroma, Pleroma.Upload,
  uploader: Pleroma.Uploaders.S3,
  base_url: "https://s3.ca-central-1.amazonaws.com"

 config :pleroma, Pleroma.Uploaders.S3,
   bucket: "moron-city-uploads",
   truncated_namespace: nil,
   streaming_enabled: true

config :pleroma, configurable_from_database: true

config :pleroma, Pleroma.Upload, filters: [Pleroma.Upload.Filter.AnonymizeFilename, Pleroma.Upload.Filter.Dedupe]