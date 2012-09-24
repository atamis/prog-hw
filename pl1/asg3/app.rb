require 'rack'
require 'json'

Suite = "suite"

class Runner
  attr_reader :results

  def initialize(binary=nil, command=nil, desugar=nil)
    @binary  ||= "osx-dist/bin/assignment3-osx"
    @switch  ||= "--test-with-desugar"
    @desugar ||= "typed-desugar-template.rkt"
    @pwd     =   Dir.pwd + "/"
  end

  def run_tests(suite=Suite)
    @results = results_hash(suite)
    @output  = `#{@binary} #{@switch} #{@desugar} #{suite}`
    self
  end

  def results_hash(suite)
    Dir["#{suite}/**/*.psl"].select { |p| File.directory?(p) }.each_with_object({}) do |path, h|
      h[path] = :passed
    end
  end

  def successes
    /(\d+) tests succeeded\./.match(@output)[1].to_i
  end

  def failures
    /(\d+) tests failed\./.match(@output)[1].to_i
  end

  def parse_test_failures
    re = /Results for (.*?\.psl).*?=== Expected stdout ===\n(.*?)\n=== Actual stdout ===\n(.*?)\n=== Expected stderr ===\n(.*?)\n=== Actual stderr ===\n(.*?\n?)\n/m
    @output.scan(re) do |m|
      file = m[0].gsub(@pwd, "")
      results[file] = {
        :exp_out => m[1],
        :got_out => m[2],
        :exp_err => m[3],
        :got_err => m[4],
      }
    end
    self
  end
end

class App
  def call(env)
    req = Rack::Request.new(env)
    case req.path_info
    when "/"
      response = Rack::Response.new
      response.write(Views::Home.new.index)
      response.finish
    when /favicon/
      response = Rack::Response.new
      response.status = 404
      response.finish
    else
      dir = req.path_info[1..-1]
      s, f, r = runner(dir)
      Rack::Response.new(Views::Results.new(dir, s, f, r).to_html)
    end
  end

  def runner(dir)
    r = Runner.new.run_tests(dir).parse_test_failures
    return r.successes, r.failures, r.results
  end
end

module Views
  class Results
    attr_reader :dir, :successes, :failures, :results

    def initialize(dir, successes, failures, results)
      @dir       = dir
      @successes = successes
      @failures  = failures
      @results   = results
    end

    def header
      "<h1>/#{dir}</h1>" +
      "<h2>#{successes} passed, #{failures} failed</h2>" +
      "<a href='#' data-path='#{dir}'>run again</a>"
    end

    def success(file)
      out = <<-html
<div class="result success">
  <span class="label label-success">pass</span>
  <span class="filename">#{file}</span>
  <div class="info row-fluid" style="display: none;">
    <div class="file-contents span6">
      <h4>#{file}</h4>
      <pre>#{File.read(file)}</pre>
    </div>
  </div>
</div>
      html
    end

    def failure(file, result)
      out = <<-html
<div class="result failure">
  <span class="label label-important">fail</span>
  <span class="filename">#{file}</span>
  <div class="info row-fluid" style="display: none;">
    <div class="file-contents span6">
      <h4>#{file}</h4>
      <pre>#{File.read(file)}</pre>
    </div>
    <div class="std span6">
      <h4>Expected on stdout</h4>
      <pre>#{result[:exp_out]}</pre>
      <h4>Actual on stdout</h4>
      <pre>#{result[:got_out]}</pre>
      <h4>Expected on stderr</h4>
      <pre>#{result[:exp_err]}</pre>
      <h4>Actual on stderr</h4>
      <pre>#{result[:got_err]}</pre>
    </div>
  </div>
</div>
      html
    end

    def to_html
      header + results.map do |file, result|
        result == :passed ? success(file) : failure(file, result)
      end.join
    end
  end


  class Home
    def initialize(suite=Suite)
      @suite = suite
    end

    def index
      <<-tmpl
<!DOCTYPE html>
<html>
  <head>
    <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/css/bootstrap.min.css" rel="stylesheet">
    <script src="//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.1/jquery.min.js"></script>
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.1.1/js/bootstrap.min.js"></script>
    <script src="https://raw.github.com/vakata/jstree/v.1.0/dist/jstree.min.js"></script>
    <title>Desugar</title>
    <style>
      body { padding-top: 20px; }
      .result { line-height: 20px; padding: 5px 0;}
      .result span { margin-right: 10px; }
      .label { display: inline-block; text-align: center; width: 40px; }
      .filename { curson: pointer; }
      #results h2 { display: inline-block; margin-right: 20px; }
    </style>
  </head>
  <body>
    <div class="container-fluid">
      <div class="row-fluid">
        <!-- Sidebar -->
        <div id="sidebar" class="span2">
          <ul class="nav nav-list">
      #{directories}
          </ul>
        </div>

        <!-- Results -->
        <div id="results" class="span10"></div>
      </div>
    </div>
    <script>
      function Runner() {
        var self = this;
        this.sidebar = $("#sidebar");
        this.results = $("#results");

        var onLinkClink = function(event) {
          var dir = $(this).data("path");
          console.log("running: ", dir);
          self.runTest(dir);
        };

        this.sidebar.on("click", "a", onLinkClink);
        this.results.on("click", "a", onLinkClink);

        this.results.on("click", ".filename", function(event) {
          $(this).siblings(".info").slideToggle(250);
        });
      }

      Runner.prototype.runTest = function(dir) {
        var self = this;
        this.results.html("<h3>Running tests in " + dir + ". Please wait...</h3");

        $.ajax({
          method: "GET",
          url: dir,
          dataType: "html",
          success: function(data) {
            self.results.html(data);
          }
        });
      };

      $(document).ready(function() {
        var fragment = location.hash.substr(1);
        var runner = new Runner();
        if (fragment) runner.runTest(fragment);
      });
    </script>
  </body>
</html>
      tmpl
    end

    def directories
      Dir["#{@suite}/**/*"]
        .select { |p| File.directory?(p) }
        .unshift(@suite)
        .map do |p|
          *dirs, name = p.split("/")
          spaces = "&nbsp;&rarr;&nbsp;" * dirs.count
          "<li><a href='##{p}' data-path='#{p}'>#{spaces}#{name}</a></li>"
        end
        .join
    end
  end
end

if __FILE__ == $0
  Rack::Server.start :app => App.new, :Port => 3456
end

