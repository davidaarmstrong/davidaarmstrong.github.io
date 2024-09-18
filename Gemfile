source 'https://rubygems.org'

git_source(:github) do |repo_name|
  repo_name = "#{repo_name}/#{repo_name}" unless repo_name.include?("/")
  "https://github.com/#{repo_name}.git"
end

gem 'github-pages', group: :jekyll_plugins

gem "webrick", "~> 1.7"

gem "jekyll", "~> 3.10"

gem "jekyll-theme-tactile", "~> 0.2.0"

gem "jekyll-commonmark-ghpages", "~> 0.5.1"
