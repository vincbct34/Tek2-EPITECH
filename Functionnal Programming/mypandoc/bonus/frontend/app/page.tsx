import FileConverterApp from '@/components/FileConverterApp';

export default function Home() {
  return (
    <main className="min-h-screen bg-gradient-to-br from-background to-muted py-8 px-4 sm:px-6 md:py-12 lg:px-8">
      <div className="max-w-7xl mx-auto">
        <header className="mb-8 text-center">
          <h1 className="text-3xl font-bold tracking-tight md:text-4xl mb-2">
            File Converter
          </h1>
          <p className="text-muted-foreground max-w-2xl mx-auto">
            Upload JSON, XML, or Markdown files, preview them, and convert them to different formats.
          </p>
        </header>
        <FileConverterApp />
      </div>
    </main>
  );
}